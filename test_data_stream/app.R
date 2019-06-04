library(shiny)

# Update w Button ---------------------------------------------------------


# ## UI stub
# ui <- fluidPage(
#     titlePanel("Iterative output demo"),
#     sidebarLayout(sidebarPanel(actionButton(
#         "start", "Start Program"
#     )),
#     mainPanel(htmlOutput("showProgressText"))))
#
# ## Server stub
# server <- function(input, output) {
#     iterativeOutput <- reactiveValues()
#
#     output$showProgressText <- renderText(paste0(sapply(
#         reactiveValuesToList(iterativeOutput), paste),
#     collapse = "<br />\n"))
#
#     observeEvent(input$start, {
#         #iterativeOutput <- reactiveValues(); # clear list
#         i <- (input$start - 1) %% 10
#
#         iterativeOutput[[as.character(i)]] <-
#             sprintf("%2d: %d", i, sample.int(1000, 1))
#
#     })
# }


# 2 -----------------------------------------------------------------------

## stub UI with an activation button and text output element
# ui <- fluidPage(
#     titlePanel("Iterative output demo"),
#     sidebarLayout(
#         sidebarPanel(
#             actionButton("start","Start Program")
#         ),
#         mainPanel(
#             textOutput("showProgressText")
#         )
#     )
# )
#
# ## server stub
# server <- function(input, output) {
#     ## reactive list to store result values
#     iterativeOutput <- reactiveValues();
#     ## text rendering function `showProgressText`
#     output$showProgressText <- renderText({
#         sapply(iterativeOutput,paste);
#     })
#     ## iterative function; modifies the reactive list on each iteration
#     observeEvent(input$start,{
#         iterativeOutput <- reactiveValues(); # clear list
#         for(i in 1:10){
#             iterativeOutput[[as.character(i)]] <-
#                 sprintf("%2d: For Demo Purpose\n", i);
#             Sys.sleep(0.2);
#         }
#     })
# }

# 3 -----------------------------------------------------------------------

# pacman::p_load(tidyverse, shiny, scales)
#
# ui <- fluidPage(
#
#     # titlePanel("New Application"),
#     #
#     # sidebarPanel(
#     #     "Progress: ",
#     #     textOutput("counter"),
#     #     hr(),
#     #     "Elapsed Time (seconds):",
#     #     textOutput("elapsed")
#     # ),
#     #
#     # mainPanel(
#     #     textOutput("x")
#     # )
#
#     mainPanel(
#         tableOutput("x"),
#         hr(),
#         textOutput("counter")
#     )
#
# )
#
#
# server <- function(input, output, session) {
#
#     maxIter <- nrow(mtcars) + 1
#
#     vals <- reactiveValues(x = data.frame(), counter = 0)
#
#     output$counter <- renderText({
#         percent(vals$counter / maxIter)
#     })
#
#     output$x <- renderTable({
#         vals$x
#     },
#     rownames = TRUE)
#
#     observe({
#         isolate({
#             rows <- tibble()
#             rows <- mtcars[vals$counter, ]
#             vals$x <- rows
#             vals$counter <- vals$counter + 1
#         })
#
#         # instance, update a text output.
#         if (isolate(vals$counter) < maxIter) {
#             invalidateLater(250, session)
#         }
#     })

    # ---- Old Things ----

    # # The number of iterations to perform
    # maxIter <- 50
    #
    # # Track the start and elapsed time
    # startTime <- Sys.time()
    # output$elapsed <- renderText({
    #     vals$x
    #     round(Sys.time() - startTime)
    # })
    #
    # # Create a reactiveValues object where we can track some extra elements
    # # reactively.
    # vals <- reactiveValues(x = 0, counter = 0)
    #
    # # Update the percentage complete
    # output$counter <- renderText({
    #     paste0(round(vals$counter/maxIter * 100, 1), "%")
    # })
    #
    # # Show the value of x
    # output$x <- renderText({
    #     round(vals$x,2)
    # })
    #
    # # Do the actual computation here.
    # observe({
    #     isolate({
    #         # This is where we do the expensive computing
    #         sum <- 0
    #         for (i in 1:100000){
    #             sum <- sum + rnorm(1)
    #         }
    #         vals$x <- vals$x + sum
    #
    #         # Increment the counter
    #         vals$counter <- vals$counter + 1
    #     })
    #
    #     # If we're not done yet, then schedule this block to execute again ASAP.
    #     # Note that we can be interrupted by other reactive updates to, for
    #     # instance, update a text output.
    #     if (isolate(vals$counter) < maxIter){
    #         invalidateLater(0, session)
    #     }
    # })

# }


# 4 -----------------------------------------------------------------------


# Packages ----------------------------------------------------------------

pacman::p_load(tidyverse,
               shiny,
               shinydashboard,
               scales,
               leaflet,
               lubridate,
               forcats,
               ggforce)


# Import ------------------------------------------------------------------

data <-
    read_rds("C:/Users/kputs/OneDrive/Data/cs-realtime/data/live_metadata.rds")

clusters <-
    read_rds("C:/Users/kputs/OneDrive/Data/cs-realtime/data/model_cluster_summary.rds")


# UI ----------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Real Time Requests"),

    # | ---- Sidebar ----
    dashboardSidebar(
        sliderInput(inputId = "sidebar_points",
                    label = "Number of Requests",
                    min = 0,
                    max = 50,
                    value = 1,
                    step = 10),

        sliderInput(inputId = "sidebar_rate",
                    label = "Processed Every # Seconds",
                    min = 0,
                    max = 2,
                    value = 1,
                    step = 0.25)
    ),

    # | ---- Body ----
    dashboardBody(

        # Prevents plot from going gray when refreshing data
        tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),

        fluidRow(
            box(title = "Samples" %>% strong(),
                width = 12,
                textOutput("counter"),
                hr(),
                leafletOutput("map"),
                hr(),
                plotOutput("hub_rides"),
                hr(),
                plotOutput("hub_fees"),
                hr(),
                plotOutput("hub_profit"),
                hr(),
                tableOutput("single"),
                hr(),
                tableOutput("data_monitor"),
                hr(),
                tableOutput("total"),
                hr()
            )
        )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

    maxIter <- nrow(data) + 1

    vals <- reactiveValues(time         = 0,
                           counter      = 0,
                           last_row     = 0,
                           data_new     = tibble(),
                           data_all     = tibble(),
                           data_monitor = tibble())

    # | Simple Output ----
    output$counter <- renderText({
        percent(vals$counter / maxIter)
    })

    output$single <- renderTable({
        vals$data_new
    })

    output$total <- renderTable({
        vals$data_all
    })

    output$data_monitor <- renderTable({
        vals$data_monitor
    })

    # | Plot Map ----
    output$map <- renderLeaflet({
        clusters %>%
            leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(lng = ~center_lon,
                             lat = ~center_lat,
                             label = ~str_c("HUB - ", hub),
                             opacity = 1,
                             weight = 1,
                             fillOpacity = 1,
                             fillColor = ~hub_color,
                             color = "black")
    })

    # | Plot Rides ----
    output$hub_rides <- renderPlot({
        vals$data_monitor %>%
            ggplot() +
            aes(x = hub,
                y = summary_n_request,
                fill = factor(busy, levels = c("Normal", "Slow", "Heavy"))) +
            geom_col(color = "black") +
            geom_errorbar(aes(ymin = n_predict + n_resid_lb,
                              ymax = n_predict + n_resid_ub),
                          width = 0.10) +
            scale_x_discrete(drop = FALSE, name = NULL) +
            scale_fill_manual(values = c("Normal" = "#91cf60", "Heavy" = "#fc8d59", "Slow" = "#ffffbf"),
                              drop = FALSE,
                              name = NULL,
                              guide = guide_legend(title.position = "top")) +
            labs(title = "Request Volume by Hour", y = "Number of Requests", x = NULL,
                 subtitle = str_c("Hour - ", vals$time$hour %>% str_pad(2, side = "left", pad = "0"), ":00"),
                 caption = "The vertical bars represent the Normal range of Request Volume") +
            theme_minimal() +
            theme(legend.position = "bottom", legend.justification = "center")
    })

    # | Plot Fees ----
    output$hub_fees <- renderPlot({
        vals$data_monitor %>%
            ggplot() +
            aes(
                y = fct_rev(hub),
                x = "A",
                fill = busy,
                size = cost_rate,
                label = str_glue("Hub {hub} - {dollar(cost_rate)} - {percent(summary_p_outlier)}")) +
            geom_label(label.padding = unit(0.50, "lines")) +
            scale_radius(range = c(4, 8), guide = "none") +
            # scale_radius(range = c(3, 6), guide = "none") +
            scale_fill_manual(values = c("Normal" = "#91cf60", "Heavy" = "#fc8d59", "Slow" = "#ffffbf"),
                              drop = FALSE,
                              name = NULL,
                              guide = "none") +
            theme_no_axes(base.theme = theme_minimal()) +
            labs(title = "Request Fee per Mile",
                 subtitle = "Fees are adjusted based on hub request volume",
                 caption = "Higher fees attract more drivers, while lower fees push drivers into other areas")
    })

    # | Plot Profit ----
    output$hub_profit <- renderPlot({
        vals$data_all %>%
            ggplot() +
            aes(x = fct_rev(hub), y = cost_profit, fill = hub) +
            geom_boxplot(varwidth = TRUE, outlier.alpha = 0.05, outlier.size = 1) +
            coord_flip() +
            scale_y_continuous(limits = c(0, 25), labels = dollar_format()) +
            scale_fill_manual(values = clusters %>% select(hub, hub_color) %>% deframe(),
                              guide = "none") +
            theme_minimal() +
            labs(x = "Hub", y = NULL, title = "Driver's Profit per Request", caption = "Box height is determined by number of requests")

    })

    # | New Data ----

    # This is my Shiny-For-Loop
    observe({
        isolate({

            row_lo <- (vals$counter * input$sidebar_points) + 1
            row_hi <- (vals$counter + 1) * input$sidebar_points

            vals$data_new <- data %>% slice(row_lo:row_hi)
            vals$last_row <- vals$data_new %>% pull(rowid) %>% max()
            vals$counter  <- vals$counter + 1

            # Aggregate the incoming data, keeping only last 3 hours
            vals$data_all <-
                bind_rows(vals$data_new, vals$data_all) %>%
                filter(lubridate::hour(datetime) >= max(lubridate::hour(datetime)) - 3)

            # Track the time of most recent data point
            vals$time <-
                vals$data_new %>%
                filter(datetime == max(datetime)) %>%
                distinct(year, month, day, hour)

            # Keep a small table listing the most recent point for each hub
            vals$data_monitor <-
                vals$data_all %>%
                arrange(desc(datetime)) %>%
                distinct(hub, .keep_all = TRUE)
        })

        # Update the map
        leafletProxy(mapId = "map",
                     data = vals$data_all) %>%
            clearShapes() %>%
            addCircles(lng = ~start_lon, lat = ~start_lat,
                       weight = 1,
                       opacity = 0.40,
                       fillOpacity = 0.40,
                       fillColor = ~hub_color,
                       color = "black",
                       radius = 200)

        # Control how often to update data
        if (isolate(vals$counter) < maxIter) {
            invalidateLater(1000 * input$sidebar_rate, session)
        }
    })
}

shinyApp(ui = ui, server = server)
