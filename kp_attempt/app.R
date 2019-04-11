library(shiny)
library(tidyverse)


# Helper Functions --------------------------------------------------------
# fx_data_raw <- function(x = NULL) {
#     if (is.null(x)) tibble(x = 1:10, y = 1:10)
# }
#
# fx_data_new <- function(x = NULL) {
#     if (is.null(x)) {
#         tibble(x = runif(n = 1, min = 0, max = 10),
#                y = rnorm(n = 1, mean = 5, sd = 2))
#     }
# }
#
# fx_data_live_initial <- function(x = NULL) {
#     reactiveVal(x)
# }
#
# fx_catch_new_data <- function(data_raw, frequency = 1000, ...) {
#     observe({
#         invalidateLater(frequency, session)
#         isolate({
#             data_new <- data_raw %>% fx_data_new()
#             if (is.null(fx_data_live_initial()))
#         })
#     })
# }


# App UI ------------------------------------------------------------------


ui <-
    shinyServer(
        fluidPage(
            # Prevents plot from going gray when refreshing data
            tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),
            plotOutput("plot")))

server <- shinyServer(function(input, output, session) {

    # Import Initial Data
    data_raw <- tibble(x = 1:10, y = 1:10)

    # Import Additional Data
    fx_new_data <- function(data) {
        tibble(x = runif(n = 1, min = 0, max = 10),
               y = rnorm(n = 1, mean = 5, sd = 2))
    }

    # Initialize live data
    data_live <- reactiveVal()

    # Observer To Catch New Data
    observe({

        # refresh every 1000ms
        invalidateLater(1000, session)

        isolate({
            new_data <- data_raw %>% fx_new_data()

            if (is.null(data_live())) {
                # When live data is empty, use the raw data
                data_live(data_raw)
            } else {
                # Add new data to initial data, keeping only X most recent points
                data_live() %>%
                    bind_rows(new_data) %>%
                    tail(20) %>%
                    data_live()
            }
        })
    })

    # Pseudo Code ----
    # iter <- 1:5
    # data_0 <- data_raw
    #
    # for (i in iter) {
    #
    #
    #     if (i <= max(iter)) {
    #
    #         str_c("Iteration", i, sep = " ") %>% rlang::inform()
    #
    #         data_0 <<-
    #             data_0 %>%
    #             slice(-1) %>%
    #             fx_new_data()
    #
    #         # print(data_0)
    #         output_plot <- data_0 %>% ggplot(aes(x = x, y = y)) + geom_point() + labs(title = i)
    #
    #         Sys.sleep(2)
    #
    #         return(output_plot)
    #     }
    # }

    # Output ----
    output$plot <- renderPlot({
        data_live() %>%
            ggplot(aes(x, y)) +
            geom_point() +
            scale_x_continuous(breaks = 0:10, limits = c(0, 10)) +
            scale_y_continuous(breaks = 0:10, limits = c(0, 10)) +
            theme_minimal()
    })

})

shinyApp(ui = ui, server = server)
