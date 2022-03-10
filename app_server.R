library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)

climate_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

server <- function(input, output) {
  #First Page
  output$text <- renderText({input$txt})
  
  #Second Page
  output$coal_plot <- renderPlotly({
    top_countries <- c("Estonia", "Australia", "Kazakhstan", "South Africa", "Czechia")
    top5_25years_coal_co2_percapita <- climate_data %>% 
      filter(country %in% top_countries) %>%
      group_by(country) %>%
      arrange(desc(year)) %>%
      slice(1:25)
    country_new <- top5_25years_coal_co2_percapita %>% filter(country %in% input$countries ) %>% 
      filter(year >= input$years[1],year <= input$years[2])
    
    coal_plot <- ggplot(country_new) + 
      geom_line(aes(x = year, y = coal_co2_per_capita, color = country)) +
      labs( title = "The trend of CO2 emission due to coal consumption per capita from 1996-2020",
            x = "Year",
            y = "CO2 emitted by coal consumption per capita",
            color = "country",
            group = 1)
    
    coal_plot <- ggplotly(coal_plot)
    
    
    return(coal_plot)
  })
  
  #Third Page
  output$text <- renderText({input$txt})
}
