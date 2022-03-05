library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)

climate_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

server <- function(input, output) {
  #First Page
  output$text <- renderText({input$txt})
  
  #Second Page
  output$plotly <- renderPlotly({
    plot_data <- country_unique <- unique(Top_CO2_coal_capita$country)
    
    
    coal_plot <- ggplot(top5_25years_coal_co2_percapita) + 
      geom_line(aes(x = year, y = coal_co2_per_capita, color = country)) +
      labs( title = "The trend of CO2 emission due to coal consumption per capita from 1996-2020",
            x = "Year",
            y = "CO2 emitted by coal consumption per capita",
            color = "country")
    
    ggplotly(coal_plot)
    
    return(coal_plot)
  })
  
  #Third Page
}
