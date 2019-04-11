set.seed(1)

library(shiny)

ui <- fluidPage(plotOutput("plotx"))

server <- function(input, output, session) {
    # A reactiveVal that holds our data
    myData <- reactiveVal()

    # Our function to get new data
    get_new_data <- function() {
        data.frame(a = sample(seq(20), 1), b = sample(seq(20), 1))
    }

    # Observer that updates the data every 1000ms.
    observe({
        # invalidate every 1000ms
        invalidateLater(1000, session)
        isolate({
            # fetch the new data
            new_data <- get_new_data()

            # If myData is empty, we initialize it with just the new data.
            if (is.null(myData()))
                myData(new_data)
            else
                # row bind the new data to the existing data, and set that as the new value.
                myData(rbind(myData(), new_data))
        })
    })

    # Plot a histrogram
    output$plotx <- renderPlot({
        hist(myData()$a)
    })
}

shinyApp(ui = ui, server = server)
