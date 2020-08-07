library(shiny)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Information about Coronavirus (COVID-19)"),
    fluidRow(
    plotlyOutput("sentimentResult")
  ),fluidRow(
    plotlyOutput("sentimentMetrics")
  ),
  fluidRow(
    column(
      width=4, 
      selectizeInput(
        "country", label=h5("Country"), choices=NULL, width="100%")
    ),
    column(
      width=4, 
      checkboxGroupInput(
        "metrics", label=h5("Selected Metrics"), 
        choices=c("Confirmed", "Deaths", "Recovered"), 
        selected=c("Confirmed", "Deaths", "Recovered"), 
        width="100%")
    )
  ),
  fluidRow(
    plotlyOutput("dailyMetrics")
  ),
  fluidRow(
    plotlyOutput("cumulatedMetrics")
  )
))