library(shiny)
library(rvest)
library(googleway)
library(DT)

source("1_load.R")

key <- "AIzaSyBp5skKkKmEO3lUzrn89kEFB3dAwjsCfwA"
set_key(key = key)


ui <- fluidPage(
  theme = bslib::bs_theme(
    bg = "#363847",
    fg = "#EEC617",
    base_font = "Roboto Mono"
  ),
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #E5E9EA}")),
  
  titlePanel("The Ultimate Scraper!"),
  
  textInput("area", "Where we looking?"),
  sliderInput("bedrooms", "How many people?", value = c(1,10), min = 1, max = 10, step = 1),
  sliderInput("pp_pw", "How much would you like to pay per week?", value = c(100, 300), min = 100, max = 300, step = 5),
  actionButton("search", "Search it!", class = "btn-lg btn-primary "),
  actionButton("scrape", "Scrape it!", class = "btn-lg btn-primary "),
  textOutput('amount'),
  DTOutput('tbl'),
  textOutput('selected'),
  downloadButton('download'),
  uiOutput('shortlist'),
  uiOutput('test')
  
)

server <- function(input, output, session) {
  
  scrape <- eventReactive(input$search, {
    area <- paste0(input$area)
    get_all_props(get_urls(area, input$bedrooms, input$pp_pw))
  })
  
  generate_table <- eventReactive(input$scrape, {
    get_all_info(scrape())
    })
  
  output$tbl<- renderDT({
    generate_table()
  })
  
  output$amount <- renderText(paste("We've found ", toString(length(scrape())), " results!", sep=""))
  
  output$selected <- renderText(input$tbl_rows_selected)
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(paste(input$area, paste0("b", input$bedrooms[1]), paste0("b", input$bedrooms[2]), paste0("p", input$pp_pw[1]), paste0("p", input$pp_pw[2]), sep="_" ), ".csv")
    },
    content = function(file) {
      write.csv(generate_table(), file)
    }
  )
  
  observeEvent(input$tbl_rows_selected, {
    for (row in input$tbl_rows_selected) {
      insertUI(
        selector = "#add",
        where = "afterEnd",
        ui = HTML(c('<img src="',generate_table()$URl[row],'">'))
      )
    }
  })
  
  output$shortlist <- renderUI({
    ui_parts <- c()
    for(i in input$tbl_rows_selected){
      ui_parts[[i]] <- fluidRow(
        column(5,
               tags$a(href=generate_table()$URl[i], generate_table()$Property[i])
               # textInput(
               #   inputId=paste0("id",i) ,
               #   label = generate_table()$Property[i]
               # )
        ),
        column(5,
               tags$img(src = get_image(generate_table()$URl[i], ctr),
                        width = 400,
                        height = 400)
        )
      )}
    ui_parts
  })
  
}
  

shinyApp(ui, server)
