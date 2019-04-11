pacman::p_load(tidyverse, shiny, ggmap, leaflet, sp, RColorBrewer, broom, rlang)

# App UI ------------------------------------------------------------------

ui <-
    shinyServer(
        fluidPage(
            # Prevents plot from going gray when refreshing data
            tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),
            plotOutput("plot"),
            leafletOutput("map"),
            verbatimTextOutput("data")
        ))


# App Server --------------------------------------------------------------

server <- shinyServer(function(input, output, session) {

    # Import Initial Data
    data_raw <-
        if ("uber_0714.rds" %in% dir()) {
            read_rds("uber_0714.rds")
        } else {
            read_csv("https://github.com/fivethirtyeight/uber-tlc-foil-response/blob/master/uber-trip-data/uber-raw-data-jul14.csv?raw=true",
                     # n_max = 100,
                     col_types = cols(`Date/Time` = col_datetime("%m/%d/%Y %H:%M:%S"),
                                      Lat = col_double(),
                                      Lon = col_double(),
                                      Base = col_character())) %>%
                arrange(`Date/Time`)
        }


    # ---- Cluster
    model_size <- 20000
    model_prop_train <- model_size / nrow(data_raw)
    model_prop_test  <- 1 - model_prop_train

    model_data <-
        data_raw %>%
        select(Lat, Lon) %>%
        modelr::resample_partition(c(train = model_prop_train, test = model_prop_test))

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
        select(k, tot.withinss, recommended_clusters, model = kclust) %>%
        print()

    model_cluster <-
        model_cluster_champion %>%
        filter(!is.na(recommended_clusters)) %>%
        pull(model) %>%
        deframe()

    plot_cluster_points <-
        model_cluster %>%
        pluck("centers") %>%
        as_tibble() %>%
        rownames_to_column("Hub") %>%
        add_column(p = model_cluster$size / model_size,
                   color = brewer.pal(length(model_cluster$size), "RdYlBu")) %>%
        arrange(p) %>%
        print()

    output$map <- renderLeaflet({
        plot_cluster_points %>%
            leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addAwesomeMarkers(
                lng = ~Lon,
                lat = ~Lat,
                label = ~str_c("Hub-", Hub),
                icon = awesomeIcons("car", library = "fa", iconColor = "yellow", markerColor = "red"))

    })

    # Initialize live data
    data_live <- reactiveVal()

    # Observer To Catch New Data
    observe({

        # refresh every 1000ms
        invalidateLater(1000, session)
        isolate({
            data_new <- data_raw %>% sample_n(1)

            if (is.null(data_live())) {
                data_live(data_new)
            } else {
                data_live() %>%
                    bind_rows(data_new) %>%
                    tail(20) %>%
                    data_live()
            }
        })
    })


    # Output ----
    output$data <- renderPrint({
        lst(data_live())
    })



    observe({
        leafletProxy("map", data = data_live()) %>%
            clearShapes() %>%
            addCircles(lng = ~Lon, lat = ~Lat)
    })
})

shinyApp(ui = ui, server = server)
