library(shiny)


ui <- fluidPage(
    actionButton("scrape", "Scrape it!"),
    actionButton("go", "Go")
)

server <- function(input, output, session) {
  
  x1 <- eventReactive(input$scrape, {
      print("nice")
    })
  
  randomVals <- eventReactive(input$go, {
    #runif(input$n)
    print("very nice")
  })
}

shinyApp(ui, server)
