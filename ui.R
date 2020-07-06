library(shiny)
library(leaflet)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
    
  headerPanel("Weather Forecast - RS, Brazil"),
  
  sidebarPanel(
                 
    br(),
    
    # value is always yyyy-mm-dd, even if the display format is different
    dateInput("day", "Date:", min="2017-03-21", max="2017-03-24", value = "2017-03-21", format = "dd/mm/yyyy"),
    
    # Simple integer interval
    sliderInput("hour", "Hour:", 
                min=0, max=23, value=0, animate=TRUE)
  
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Temperature Graph", plotOutput("plot"), tableOutput("dadostemp")),
      tabPanel("Radiation Graph", plotOutput("plot2"), tableOutput("dadosrad")),
      tabPanel("Wind Graph", plotOutput("plot3"), tableOutput("dadosvento")),
      tabPanel("Humidity Graph", plotOutput("plot4"), tableOutput("dadosumidade")),
      tabPanel("Rain Graph", plotOutput("plot5"), tableOutput("dadoschuva")),
      tabPanel("Map", leafletOutput("map"))))
))
