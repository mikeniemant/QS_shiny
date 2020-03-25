# QS results dashboard - 040 - server

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(dplyr)
library(ggplot2)
library(tidyr)
library(xlsx)
library(DT)

# import global variables and functions
source("./global.R")

# UI ----
ui <- fluidPage(
  # App title
  titlePanel(title = "V 0.4.0: test conditional panel"),
  
  fluidRow(
    # Inputs: edit in this file:
    column(4,
           source("./ui_sidebar.R")$value),
    
    # Outputs: edit in these files:
    column(8,
           tabsetPanel(id = "tabs",
                       source("./ui_tab_1.R")$value,
                       source("./ui_tab_2.R")$value)
    )
  )
)

# All functionality in the back-end
# Server ----
server <- shinyServer(function(input, output, session) {
})

# Run application 
shinyApp(ui = ui, server = server)