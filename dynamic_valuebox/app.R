library(shiny)
library(shinydashboard)

ui = dashboardPage(
    dashboardHeader(title = "Dynamic number of valueBoxes"),
    dashboardSidebar(
        selectInput(inputId = "choosevar",
                    label = "Choose Cut Variable:",
                    choices = c("Nr. of Gears"="gear", "Nr. of Carburators"="carb"))
    ),
    dashboardBody(
        uiOutput("plots")
    )

)

server <- function(input, output) {
    #dynamically create the right number of htmlOutput
    # renderUI
    output$plots <- renderUI({
        plot_output_list <- lapply(unique(mtcars[,input$choosevar]), function(i) {
            plotname <- paste0("plot", i)
            valueBoxOutput(plotname, width = 3)
            # htmlOutput(plotname)
        })

        tagList(plot_output_list)
    })

    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.

    for (i in 1:max(unique(mtcars[,"gear"]),unique(mtcars[,"carb"]))) {
        local({
            my_i <- i
            plotname <- paste0("plot", my_i)

            output[[plotname]] <- renderUI({
                valueBox(
                    input$choosevar,
                    my_i,
                    icon = icon("credit-card")
                )
            })
        })
    }
}

# Run the application
shinyApp(ui = ui, server = server)
