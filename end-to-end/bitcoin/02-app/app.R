library(DBI)
library(dygraphs)
library(xts)
library(shiny)
library(shinythemes)
library(dplyr)
library(dbplyr)
library(pool)

con <- pool::dbPool(odbc::odbc(), dsn = "Postgres (DSN)")
bitcoin <- tbl(con, "bitcoin")

codes <- c("USD", "JPY", "CNY", "SGD", "HKD", "CAD", "NZD", 
           "AUD", "CLP", "GBP", "DKK", "SEK", "ISK", "CHF", 
           "BRL", "EUR", "RUB", "PLN", "THB", "KRW", "TWD")

ui <- fluidPage(
  theme = shinytheme("paper"),
  titlePanel("Bitcoin Exchange Rates"),
  sidebarLayout(
    sidebarPanel(
      selectInput("code", "Exchange Code:", choices = codes),
      dateRangeInput('dates',
                     label = 'Date range:',
                     start = Sys.Date() - 3, 
                     end = Sys.Date()
      )
    ),
    mainPanel(align="center",
      dygraphOutput("dygraph", width = "100%", height = "275px"),
      p(),
      textOutput("asof"),
      verbatimTextOutput("dateRangeText")
    )
  )
)

server <- function(input, output) {

  dat <- reactive({
    validate(need(input$dates[1] < input$dates[2], "Start must occur before end"))
    start <- input$dates[1]
    end <- input$dates[2] + 1
    bitcoin %>%
      filter(name == input$code) %>%
      filter(timestamp > start & timestamp <= end) %>%
      select(timestamp, last, symbol) %>%
      collect
  })
  
  tseries <- reactive({
    xts(dat()$last, dat()$timestamp)
  })
  
  lab <- reactive({
    paste0("Bitcoin (", dat()$symbol[1], ")")
  })

  output$dygraph <- renderDygraph({
    dygraph(tseries(), main = lab()) %>%
      dyOptions(axisLineWidth = 1.5, 
                fillGraph = TRUE, 
                drawGrid = FALSE, 
                colors = "steelblue", 
                axisLineColor = "darkgrey", 
                axisLabelFontSize = 15) %>%
      dyRangeSelector(fillColor = "lightsteelblue", strokeColor = "white")
  })
  
  output$asof <- renderText({
    paste("Data as of:", as.character(max(dat()$timestamp)))
  })
  
}

shinyApp(ui = ui, server = server)
