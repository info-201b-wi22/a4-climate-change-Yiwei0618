library(shiny)   #for hosting shiny website
library(ggplot2) #for plot with ggplot
library(dplyr)   #for data manipulation
library(plotly)  #for plot with plot_ly

source("app_ui.R")
source("app_server.R")

shinyApp(ui = ui, server = server)


