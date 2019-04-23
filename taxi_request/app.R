pacman::p_load(tidyverse,
               shiny,
               ggmap,
               leaflet,
               sp,
               foreach,
               RColorBrewer,
               broom,
               rlang,
               flexclust,
               shinydashboard,
               DT,
               formattable,
               feather)

inform(Sys.time())


# Helper Functions --------------------------------------------------------

vlookup <- function(match_value, data, match_column, return_value) {
    match <- match(match_value, data[[match_column]])
    data[[return_value]][match]
}

# App UI ------------------------------------------------------------------

ui <-
    dashboardPage(
        dashboardHeader(title = "Real Time Requests"),
        dashboardSidebar(disable = TRUE),
        dashboardBody(

            # Prevents plot from going gray when refreshing data
            tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),

            fluidRow(
                box(title = "Live Requests" %>% strong(),
                    leafletOutput("map"),
                    width = 12)
            ),

            fluidRow(
                box(title = "Distance Monitor" %>% strong(),
                    width = 6,
                    plotOutput("plot_distance"),
                    hr(),
                    valueBoxOutput("box_outlier", width = 6)),

                box(title = "Request Monitor" %>% strong(),
                    width = 6,
                    plotOutput("plot_requests"),
                    hr(),
                    radioButtons(inputId = "buttons",
                                 label = "Distribution of Requests",
                                 choices = c("Normal", "Chaos"),
                                 selected = "Normal",
                                 inline = TRUE)


                    # hr(),
                    # valueBoxOutput("box_1", width = 3),
                    # valueBoxOutput("box_2", width = 3),
                    # valueBoxOutput("box_3", width = 3),
                    # valueBoxOutput("box_4", width = 3),
                    # valueBoxOutput("box_5", width = 3),
                    # valueBoxOutput("box_6", width = 3),
                    # valueBoxOutput("box_7", width = 3),
                    # valueBoxOutput("box_8", width = 3),
                    # valueBoxOutput("box_9", width = 3),
                    # hr()
                )
            ),

            fluidRow(
                box(title = "Debug Menu",
                    width = 12,
                    collapsed = TRUE,
                    collapsible = TRUE,
                    verbatimTextOutput("data_list"))
            )

        )
    )


# App Server --------------------------------------------------------------

server <- shinyServer(function(input, output, session) {

    # ---- Import Initial Data ----

    inform("loading data")

    data_raw <-
        if ("uber_0714.feather" %in% dir()) {
            read_feather("uber_0714.feather")
        } else {
            read_csv("https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-jul14.csv?raw=true",
                     # n_max = 100,
                     col_types = cols(`Date/Time` = col_datetime("%m/%d/%Y %H:%M:%S"),
                                      Lat = col_double(),
                                      Lon = col_double(),
                                      Base = col_character())) %>%
                arrange(`Date/Time`)
        }


    # ---- Cluster ----
    inform("building clusters")

    model_size <- 20000
    model_proportion <- model_size / nrow(data_raw)
    model_prop_holdout  <- 1 - (model_proportion * 3)

    model_data <-
        data_raw %>%
        select(Lat, Lon) %>%
        modelr::resample_partition(c(train = model_proportion,
                                     test  = model_proportion,
                                     live  = model_proportion,
                                     hold  = model_prop_holdout))

    model_cluster_list <-
        tibble(k = 1:10) %>%
        mutate(kclust = map(k, ~kmeans(as_tibble(model_data$train), .x)),
               glance = map(kclust, glance))

    model_cluster_champion <-
        model_cluster_list %>%
        unnest(glance) %>%
        arrange(k) %>%
        mutate(rate = 100 * (tot.withinss - lag(tot.withinss)) / lag(tot.withinss)) %>%
        mutate(mark_potential = ifelse(rate < lag(rate), 1, 0)) %>%
        group_by(mark_potential) %>%
        mutate(mark_count = sequence(n()),
               recommended_clusters = ifelse(mark_potential == 1 & mark_count == 1, k, NA)) %>%
        ungroup() %>%
        select(k, tot.withinss, recommended_clusters, model = kclust)

    model_cluster_k <-
        model_cluster_champion %>%
        filter(!is.na(recommended_clusters)) %>%
        pull(k)

    model_cluster <-
        kcca(model_data$train %>% as_tibble(),
             k = model_cluster_k,
             family = kccaFamily("kmeans"))

    model_cluster_centers <-
        model_cluster %>%
        attr("centers") %>%
        as_tibble() %>%
        rowid_to_column("cluster")

    model_cluster_assignments <-
        model_data$test %>%
        as_tibble() %>%
        mutate(cluster = predict(model_cluster, .))

    model_cluster_distances <-
        left_join(model_cluster_assignments,
                  model_cluster_centers,
                  by = "cluster",
                  suffix = c(".point", ".center")) %>%
        rowwise() %>%
        mutate(distance = geosphere::distm(c(Lon.point, Lat.point), c(Lon.center, Lat.center))) %>%
        ungroup()

    model_cluster_summary <-
        model_cluster_distances %>%
        group_by(cluster) %>%
        summarise(Lat = unique(Lat.center),
                  Lon = unique(Lon.center),
                  count = n(),
                  d_avg = mean(distance),
                  d_med = median(distance),
                  d_iqr = IQR(distance),
                  d_out = if_else(distance > (d_med + d_iqr * 2), 1, 0) %>% mean()) %>%
        mutate(p = count / sum(count)) %>%
        arrange(p) %>%
        mutate(Hub = letters[sequence(n())] %>% str_to_upper() %>% as_factor(),
               Color = brewer.pal(n(), "Set1"))

    model_cluster_outlier <- model_cluster_summary$d_out %>% mean()

    # ---- Leaflet: Map Base ----
    inform("base map")

    output$map <- renderLeaflet({
        model_cluster_summary %>%
            leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(lng = ~Lon,
                             lat = ~Lat,
                             label = ~str_c("HUB - ", Hub),
                             opacity = 1,
                             weight = 1,
                             fillOpacity = 1,
                             fillColor = ~Color,
                             color = "black")

    })

    # ---- Reactive: Live Data ----
    data_stream <- reactiveVal()
    data_stream_summary <- reactiveVal()

    data_streaming_max <- 500
    data_streaming_refresh <- 3000

    # ---- Button: Distribution ----
    data_group <- reactive({
        switch(input$buttons,
               Normal = NULL,
               Chaos = "cluster")
    })

    data_streaming_n <- reactive({
        switch(input$buttons,
               Normal = 20,
               Chaos = 10)
    })


    # ---- Live: Distribution Adjustment ----
    # NOT REPRESENTATIVE OF LIVE STREAMING SITUATION
    data_stream_full <- reactive({
        model_data$live %>%
            as_tibble() %>%
            add_column(cluster = predict(model_cluster, .)) %>%
            group_by(!!! syms(data_group()))
    })


    # ---- Live: Stream ----
    observe({

        # refresh after x miliseconds
        invalidateLater(data_streaming_refresh, session)

        isolate({

            # Live: Score New Data Points
            data_new <-

                # - TESTING -
                # test_data %>%
                # sample_n(20) %>%
                # add_column(cluster = predict(model_cluster, .)) %>%

                # - ACTUAL -
                data_stream_full() %>%
                sample_n(data_streaming_n()) %>%
                ungroup() %>%

                # add_column(cluster = predict(model_cluster, .)) %>%            #--placeholder for actual cluster assignment in live process
                left_join(model_cluster_summary,
                          by = "cluster",
                          suffix = c(".point", ".center")) %>%
                rowwise() %>%
                mutate(distance = geosphere::distm(c(Lon.point, Lat.point),
                                                   c(Lon.center, Lat.center)),
                       minutes = distance / 671) %>%
                ungroup() %>%
                mutate(d_outlier = if_else(distance > (d_med + 2*d_iqr), 1, 0))



            # Live: Update Existing Data
            if (is.null(data_stream())) {
                data_new %>% data_stream()                                       # if stream is empty, we start it
            } else {
                data_stream() %>%                                                # if stream is active, we add to it
                    bind_rows(data_new) %>%
                    tail(data_streaming_max) %>%
                    data_stream()
            }
        })


        # ---- Live: Summary ----
        isolate({

            # fx_live_summary_help <- function(data) {
            #     data %>%
            #         ungroup() %>%
            #         mutate(dist_avg = dist_total / count,
            #                p_obs = count / sum(count)) %>%
            #         left_join(model_cluster_summary %>% select(Hub, p_exp = p), by = "Hub") %>%
            #         mutate(prop_diff = p_exp - p_obs,
            #                prop_thrs = p_exp * 0.30,
            #                prop_warn = ifelse(abs(prop_diff) > prop_thrs, 1, 0),
            #                prop_sign = ifelse(prop_diff > 0, 1, -1),
            #                alert = prop_warn * prop_sign)
            # }


            # model_cluster_summary
            # data_new



            # New Data Summary
            data_summary_new <-
                data_new %>%
                group_by(Hub) %>%
                summarise(count = n(),
                          outlier_sum = sum(d_outlier, na.rm = TRUE)) %>%
                mutate(p = count / sum(count))

            # data_summary_full <-


                # left_join(model_cluster_summary, ., by = "Hub", suffix = c(".hub", ".new"))

            # data_summary <-
            #     data_new %>%
            #     group_by(Hub) %>%
            #     summarise(count = n(),
            #               dist_total = sum(distance)) %>%
            #     fx_live_summary_help()

            # Create and Update Summary
            if (is.null(data_stream_summary)) {
                data_summary_new %>%
                    data_stream_summary()
            } else {
                data_stream_summary() %>%
                    bind_rows(data_summary_new) %>%
                    group_by(Hub) %>%
                    summarise(count = sum(count),
                              outlier_sum = sum(outlier_sum, na.rm = TRUE)) %>%
                    ungroup() %>%
                    mutate(p = count / sum(count)) %>%
                    data_stream_summary()

                # data_stream_summary() %>%
                #     bind_rows(data_summary) %>%
                #     group_by(Hub) %>%
                #     summarise(count = sum(count),
                #               dist_total = sum(dist_total)) %>%
                #     fx_live_summary_help() %>%
                #     data_stream_summary()
            }

        })
    })


    # ---- Summary Output ----
    # output$table_summary <- renderDataTable(server = TRUE, {
    #
    #     data_stream_summary() %>%
    #         datatable(extensions = c('Scroller'),
    #                   rownames = FALSE,
    #                   options = list(dom = 't', scrollY = 300, scroller = TRUE, scrollX = TRUE)) %>%
    #         DT::formatRound(columns = 2:9)
    # })
    #
    # observe({
    #     reloadData(dataTableProxy("table_summary"),
    #                resetPaging = FALSE,
    #                clearSelection = "none")
    # })

    # ---- Update Live Map ----
    observe({
        leafletProxy(mapId = "map",
                     data = data_stream()) %>%
            clearShapes() %>%
            addCircles(lng = ~Lon.point, lat = ~Lat.point,
                       weight = 1,
                       opacity = 0.40,
                       fillOpacity = 0.40,
                       fillColor = ~Color,
                       color = "black",
                       radius = 200)
    })


    # ---- Plot: Distance Distribution ----
    # Unit is in minutes.  Conversion is 1/671 meters per minute.  13,420 meters in 20 mins
    output$plot_distance <- renderPlot({
        data_stream() %>%
            filter(!is.na(minutes)) %>%
            ggplot() +
            aes(x = minutes, y = Hub, fill = Hub) +
            ggridges::geom_density_ridges() +
            scale_x_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20), labels = scales::unit_format(unit = "mins")) +
            ggridges::theme_ridges() +
            labs(y = NULL, x = NULL, title = "Time to Pickup, from Hub", caption = expression(italic("Assuming 25 MPH Average"))) +
            scale_fill_manual(values = model_cluster_summary %>% select(Hub, Color) %>% deframe(),
                              guide = guide_legend(title.position = "top")) +
            theme(legend.position = "bottom", legend.justification = "center")
    })

    # ---- Plot: Request Distribution ----
    output$plot_requests <- renderPlot({

        data_stream_summary() %>%
            mutate(p_exp = vlookup(data = model_cluster_summary, match_value = Hub, match_column = "Hub", return_value = "p"),
                   p_dif = p_exp - p,
                   d_magnitude = abs(p_dif),
                   d_sign = ifelse(p_dif > 0, 1, -1),
                   d_threshold_factor = case_when(p < 0.05 ~ 1,
                                                  p < 0.10 ~ 0.75,
                                                  p < 0.20 ~ 0.50,
                                                  TRUE     ~ 0.20),
                   d_magnitude_thr = p * d_threshold_factor,
                   d_warn = (d_magnitude > d_magnitude_thr) * d_sign,
                   d_color = case_when(d_warn == -1 ~ "Heavy",
                                       d_warn ==  1 ~ "Slow",
                                       d_warn ==  0 ~ "Normal") %>%
                       factor(levels = c("Normal", "Slow", "Heavy"))
            ) %>%

            ggplot(aes(x = Hub, y = p, fill = d_color)) +
            geom_col(color = "black") +
            coord_flip() +
            # geom_text(aes(label = scales::number(count, big.mark = ",")), nudge_y = 0.025, size = 3) +
            ggridges::theme_ridges() +
            scale_y_continuous(name = NULL, limits = c(0, 0.60), labels = scales::percent_format()) +
            scale_fill_manual(values = c("Normal" = "#91cf60", "Heavy" = "#fc8d59", "Slow" = "#ffffbf"),
                              drop = FALSE,
                              name = "Live vs. Expected Requests",
                              guide = guide_legend(title.position = "top")) +
            theme(legend.position = "bottom", legend.justification = "center") +
            labs(title = "Percent of All Requests by Hub", x = NULL)

    })


    # ---- Box: Outlier Detection ----

    value_outliers <- reactive({
        data_stream_summary() %>%
            summarise(obs_outliers = sum(outlier_sum) / sum(count)) %>%
            mutate(exp_outliers = model_cluster_outlier,
                   alert = obs_outliers > exp_outliers)
    })

    output$box_outlier <- renderValueBox({
        valueBox(value = value_outliers()$obs_outliers %>% scales::percent(),
                 subtitle = "Distance Outlier Detection",
                 color = ifelse(value_outliers()$alert, "orange", "olive"))
    })

    # ---- Distribution Plotly ----
    # output$plot_plotly <- renderPlotly({
    #
    #     fx_plotly_helper <- function(data) {
    #         data %>%
    #             plot_ly(x = ~distance) %>%
    #             # add_histogram(histnorm = "probability", nbinsx = 20) %>%
    #             add_histogram(histnorm = "probability") %>%
    #             add_annotations(~unique(Hub), x = 0.50, y = 1, xref = "paper", yref = "paper", showarrow = FALSE)
    #     }
    #
    #     data_stream() %>%
    #         mutate(Hub_1 = Hub) %>%
    #         nest(-Hub_1) %>%
    #         mutate(myplot = map(data, fx_plotly_helper)) %>%
    #         pull(myplot) %>%
    #         subplot(nrows = n_distinct(data_stream()$Hub), shareX = TRUE, shareY = TRUE)
    #
    # })


    # ---- Value Boxes ----

    # output$box_test <- renderValueBox({
    #     valueBox(
    #         value = str_c("Hub ", data_stream_summary()$Hub[[1]]),
    #         subtitle = str_glue("Requests: {data_stream_summary()$count[[1]] %>% scales::number(big.mark = ",")} ({data_stream_summary()$p_obs[[1]] %>% scales::percent()})"),
    #         color = ifelse(data_stream_summary()$p_obs[[1]] >= 0.40, "light-blue", "orange")
    #     )
    # })


    # fx_hub_box <- function(i) {
    #         renderValueBox({
    #             valueBox(
    #                 value = str_c("Hub ", data_stream_summary()$Hub[[i]]),
    #                 subtitle = str_glue("Requests: {data_stream_summary()$count[[i]] %>% scales::number(big.mark = ",")} ({data_stream_summary()$p_obs[[i]] %>% scales::percent()})"),
    #                 color = case_when(
    #                     data_stream_summary()$alert[[i]] == -1 ~ "red",
    #                     data_stream_summary()$alert[[i]] ==  1 ~ "yellow",
    #                     data_stream_summary()$alert[[i]] ==  0 ~ "olive")
    #             )
    #         })
    # }
    #
    # output$box_1 <- fx_hub_box(1)
    # output$box_2 <- fx_hub_box(2)
    # output$box_3 <- fx_hub_box(3)
    # output$box_4 <- fx_hub_box(4)
    # output$box_5 <- fx_hub_box(5)
    # output$box_6 <- fx_hub_box(6)
    # output$box_7 <- fx_hub_box(7)
    # output$box_8 <- fx_hub_box(8)
    # output$box_9 <- fx_hub_box(9)


    # Count the number of rows in the summary table
    # and create a placeholder in the UI for each row
    # box_names <- reactive({
    #     data_stream_summary() %>%
    #         nrow() %>%
    #         seq(1) %>%
    #         str_c("hub_box_", .)
    # })

    # output$value_box_ui <- renderUI({
    #     box_names() %>%
    #         map(valueBoxOutput, width = 3) %>%
    #         tagList()
    # })


    # THIS ONE WORKS
    # for (i in 1:10) {
    #     local({
    #         output_name <- str_c("hub_box_", i)
    #         output[[output_name]] <- renderValueBox({
    #             valueBox(data_stream_summary()[i, "Hub"] %>% str_c("Hub ", .), 100)
    #         })
    #     })
    # }

    # foreach(i = 1:10) %do% {
    #
    #     local({
    #         name_box <- str_c("hub_box_", i)
    #         name_hub <- data_stream_summary()$Hub[i]
    #
    #         value_prop_obs <- data_stream_summary()$p_obs[i] %>% scales::percent()
    #         value_count <- data_stream_summary()$count[i] %>% scales::number(big.mark = ",")
    #         value_color <- data_stream_summary()$p_obs[i]
    #
    #
    #         output[[name_box]] <- renderUI({
    #             valueBox(
    #                 value = str_c("HUB ", name_hub),
    #                 subtitle = str_glue("Requests: {value_count} ({value_prop_obs})")
    #             )
    #         })
    #     })
    # }


    # ---- Data Output ----
    output$data_list <- renderPrint({
        lst(
            data_stream = data_stream() %>% select(Lat.point, Lon.point, Hub, distance, d_outlier),
            summary_live = data_stream_summary(),
            outliers = value_outliers(),
            button = input$buttons,
            group = data_group(),
            sum_n = data_stream_summary()$count %>% sum(),
            new_n = data_streaming_n()
        )
    })

})

shinyApp(ui = ui, server = server)
