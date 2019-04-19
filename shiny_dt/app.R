library(shiny)
library(DT)

ui <- fluidPage(dataTableOutput("table"))

server <- function(input,output,session){
    values <- reactiveValues()
    pollData <- reactivePoll(4000, session,
                             checkFunc=function(){
                                 Sys.time()
                             },
                             valueFunc=function(){
                                 data.frame(a=sample(c("a","b","c"),3),b=runif(3),c=runif(3),stringsAsFactors = F)
                             })

    output$table <- renderDataTable({ pollData()})

    observe({
        values$selected <- pollData()$a[input$table_rows_selected]
    })

    proxy = dataTableProxy('table')
    observeEvent(pollData(),{
        selectRows(proxy, which(pollData()$a %in% values$selected))
    })
}

shinyApp(ui,server)
