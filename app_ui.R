library(ggplot2)
library(plotly)
library(dplyr)
library(bslib)
climate_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

#Data needed for intro page and input widgets

#Rank of countries that have CO2 emission due to highest coal consumption per capita:
Rank_CO2_coal_capita <- climate_data %>% 
  group_by(country) %>%
  arrange(desc(year)) %>%
  filter(year >= 1996 & year <= 2020) %>%
  summarize(average_coal_co2_per_capita = mean(coal_co2_per_capita, na.rm =TRUE))

Top_CO2_coal_capita <- climate_data %>% 
  group_by(country) %>%
  arrange(desc(year)) %>%
  filter(year >= 1996 & year <= 2020) %>%
  summarize(average_coal_co2_per_capita = mean(coal_co2_per_capita, na.rm =TRUE)) %>% 
  arrange(desc(average_coal_co2_per_capita)) %>% 
  slice(1:5)%>% 
  select(country)


#top_countries <- c("Estonia, Australia, Kazakhstan, South Africa, and Czechia")

#Country
#country_unique <- unique(Top_CO2_coal_capita$country)


#Year Range
year_for_range <- climate_data %>% 
  group_by(country) %>%
  arrange(desc(year)) %>%
  filter(year >= 1996 & year <= 2020)
year_range <- range(year_for_range$year)
year_range[1]
#Make a new data frame of CO2 emission due to coal consumption in most recent 25 years
#top5_25years_coal_co2_percapita <- climate_data %>% 
  #group_by(country) %>%
  #arrange(desc(year)) %>%
  #filter(year >= 1996 & year <= 2020)

#Data frame for the plot
top_countries <- c("Estonia", "Australia", "Kazakhstan", "South Africa", "Czechia")
top5_25years_coal_co2_percapita <- climate_data %>% 
  filter(country %in% top_countries) %>%
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


#Average for top5 countries
avg_co2 <- top5_25years_coal_co2_percapita %>%  
  summarize(average_co2_per_capita_topcountries = mean(coal_co2_per_capita, na.rm =TRUE))

avg_co2_country <- avg_co2 %>% 
  summarise(avg_top = mean(average_co2_per_capita_topcountries, na.rm =TRUE))%>% 
  pull(avg_top)



# ==========================
# 1.Intro Page
# ==========================



# ============================
# 2. WIDGETS FOR SECOND PLOT
# ============================

country_check <- checkboxGroupInput(inputId = "countries",
                                  label = "Country for Selection",
                                  choices = unique(Top_CO2_coal_capita$country),
                                  selected = "South Africa")

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


page_one <- tabPanel(
  "Introduction",
  mainPanel(
    ui <- fluidPage(
      h1("Introduction"),
         p(paste0("Climate change has been an urgent topic in recent years. 
                  Not only the change of temperature on earth will impact lives, 
                  but it also brings various consequences. 
                  For this project, I want to explore the sources of climate change,
                  which is usually caused by the emission of greenhouse gas such as CO2. 
                  The data I am going to use are collected by Our World in Data. 
                  The mission of this organization is to make data and research on the world’s most significant problems understandable and accessible. 
                  Thus, this detest collected deep insight into climate change due to GHG emissions. 
                  The limitation of this dataset is that there are missing values in specific columns, which make it hard to compare factors thoroughly. 
                  Another limitation is that the most recent available data is from 2020, which is entirely updated but not most updated. 
                  I will focus my exploring topic on the amount of CO2 emitted by coal consumption and the emission by different countries.
                  I chose to use the value of CO2 emission due to coal consumption per capita, 
                  and I think it will be more accurate to use the average emission rate per person since it can eliminate bias such as the size of the country. 
                  I calculated the top 5 countries with the highest CO2 emission due to coal consumption per capita in the 25 years from ", year_range[1], " to " , year_range[2] , " and they are Estonia, Australia, Kazakhstan, South Africa, and Czechia " ,
                  max_co2_per_capita_country, " has the highest emission value, which is ", max_co2_per_capita, " tonnes per person each year. The average CO2 emmision due to coal combustion among these five countries is " , avg_co2_country ,
                  " tonnes per person each year.I compared the trend of the CO2 emission due to coal consumption per capita of these five countries from 1996-2020 using a line plot with interactive widgets."))
    )
  )
)

        
              
          
                  
                  
                  
page_two <- tabPanel("Interactive Visualization",
                           sidebarLayout(
                             sidebarPanel(
                               country_check <- checkboxGroupInput(inputId = "countries",
                                                                   label = "Country for Selection",
                                                                   choices = unique(top5_25years_coal_co2_percapita$country),
                                                                   selected = "South Africa"),
                               year_slider <- sliderInput(inputId = "years",
                                           label = "Year Range Selection",
                                           min = year_range[1],
                                           max = year_range[2],
                                           value = year_range,
                                           step = 1)
                               ),
                             mainPanel(
                               ui <- fluidPage(
                                 h1("The trend of CO2 emission due to coal consumption per capita from 1996-2020"),
                                 plotlyOutput("coal_plot")
                             )
                             )
                        )
)

page_three <- tabPanel(
  "Value-Sensitive Page",
  mainPanel(
    ui <- fluidPage( h1("What's direct stakeholders of climate change?"),
      p(paste0("The direct stakeholders of climate change are so many! In a broad scope, it can be all human beings in this world and the whole environment. Climate change will affect humans because the abnormal temperature will cause unexpected disasters. For example, in February 2021, the winter storm in Texas had caused a power outage in many regions. In Washington State, many regions experience heat waves in summer. This extreme weather can pose a potential health risk for the human being, especially socially vulnerable groups. Meanwhile, it can damage the environment, such as wildfires and the disappearance of Arctic iceberg."))
    )
  )
)


ui <- navbarPage(
  theme = bs_theme(bootswatch = "slate"),
  "Climate Change Project",
  page_one,
  page_two,
  page_three
)                              
                               
                               