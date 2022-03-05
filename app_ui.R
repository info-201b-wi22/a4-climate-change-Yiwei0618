library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)
climate_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

#Data needed for intro page and input widgets

#Countries that have CO2 emission due to highest coal consumption per capita:
Rank_CO2_coal_capita <- climate_data %>% 
  group_by(country) %>%
  arrange(desc(year)) %>%
  filter(year >= 1996 & year <= 2020) %>%
  summarize(average_coal_co2_per_capita = mean(coal_co2_per_capita, na.rm =TRUE))

Top_CO2_coal_capita <- Rank_CO2_coal_capita %>% 
  arrange(desc(average_coal_co2_per_capita)) %>% 
  slice(1:5)



#Country
country_unique <- unique(Top_CO2_coal_capita$country)


#Year Range
year_for_range <- climate_data %>% 
  group_by(country) %>%
  arrange(desc(year)) %>%
  filter(year >= 1996 & year <= 2020)
year_range <- range(year_for_range$year)

#Make a new data frame of CO2 emission due to coal consumption in most recent 25 years
top5_25years_coal_co2_percapita <- climate_data %>% 
  group_by(country) %>%
  arrange(desc(year)) %>%
  filter(year >= 1996 & year <= 2020)

#Data frame for the plot
top5_countries_df <- climate_data %>% filter(country %in% top5_countries)
top5_25years_coal_co2_percapita <- top5_countries_df %>%
  group_by(country) %>%
  arrange(desc(year)) %>%
  slice(1:25)

#maximum value&country
max_co2_per_capita <- Rank_CO2_coal_capita %>%
  filter(average_coal_co2_per_capita == max(average_coal_co2_per_capita, na.rm = TRUE))%>%
  pull(average_coal_co2_per_capita)

max_co2_per_capita_country <-Rank_CO2_coal_capita %>%
  filter(average_coal_co2_per_capita == max(average_coal_co2_per_capita, na.rm = TRUE))%>%
  select(country)


# ==========================
# 1.Intro Page
# ==========================



# ============================
# 2. WIDGETS FOR SECOND PLOT
# ============================

country_check <- checkboxGroupInput(inputId = "countries",
                                  label = "Country for Selection",
                                  choices = country_unique,
                                  selected = country_unique)

year_slider <- sliderInput(inputId = "years",
                            label = "Year Range Selection",
                            min = year_range[1],
                            max = year_range[2],
                            value = year_range,
                            step = 1)


# ============================
# 3.Value Sensitive Page
# ============================


# ============
# 4. UI PAGES
# ============
ui <- fluidPage(h1(strong("My Project"),
                   style = "color: Green"),
                
                # use tabsetPanel layout
                tabsetPanel("Introduction and Dataset",
                            mainPanel(textOutput("Climate change has been an urgent topic in recent years. Not only the change of 
                                                 temperature on earth will impact lives, but it also brings various consequences.
                                                 For this project, I want to explore the sources of climate change, which is usually caused by the emission of greenhouse gas such as CO2. 
                                                 The data I am going to use are collected by Our World in Data. The mission of this organization is to make data and research on the world’s most significant problems understandable and accessible. 
                                                 Thus, this detest collected deep insight into climate change due to GHG emissions. 
                                                 The limitation of this dataset is that there are missing values in specific columns, which make it hard to compare factors thoroughly. 
                                                 Another limitation is that the most recent available data is from 2020, which is entirely updated but not most updated.
                                                 I will focus my exploring topic on the amount of CO2 emitted by coal consumption and the emission by different countries. 
                                                 I chose to use the value of CO2 emission due to coal consumption per capita, and I think it will be more accurate to use the average emission rate per person since it can eliminate bias such as the size of the country.
                                                 I calculated the top 5 countries with the highest CO2 emission due to coal consumption per capita in the 25 years(1996-2020) and compiled them into a data frame. Estonia, Australia, Kazakhstan, South Africa, and Czechia are the five countries. 
                                                 Estonia has the highest emission value, 8.87 tonnes per person each year. I compared the trend of the CO2 emission due to coal consumption per capita of these five countries from 1996-2020 using a line plot with interactive widgets.")
                            ) ),
              
          
                  
                  
                  
                 tabPanel("Interactive Visualization",
                           sidebarLayout(#use sidebarLayout for first page
                             sidebarPanel(country_check,
                                          year_slider),
                             mainPanel(plotlyOutput("coal_plot"))
                           )),
               
                )




