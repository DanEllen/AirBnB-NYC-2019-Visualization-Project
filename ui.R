#Libraries ------------------------

library(shinydashboard)#Makes it easy to create Shiny dashboards
library(shiny)#Create html app easily
library(leaflet)#Leaflet is one of the most popular open-source JavaScript libraries for 
#interactive maps. This leaflet R package makes it easy to integrate and control
#Leaflet maps in R. 
library(leaflet.extras) #Provides a simple heat map function, addheatmap().
library(tidyverse) #Several tools to organize data
library(ggplot2) #Several tools for graphing
library(treemapify) # Allows easy creation of Treemap graph
library(scales) #Allows easily labeling of axis. Example:
#scale_x_continuous(labels = unit_format(unit = "K", scale = 1e-3))
library(lubridate) #Allows easy extraction of day, month, year

## 1. Header -------------------------------

header = dashboardHeader( title = HTML("Airbnb Market NYC"), 
                          disable = FALSE)

## 2. Siderbar ------------------------------

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    menuItem("Time series", tabName = "timeseries", icon = icon("chart-line")),
    menuItem("Snapshop", tabName = "snapshot", icon = icon("chart-bar")),
    menuItem("Share", tabName = "share", icon = icon("share-alt"))
    
  )
)

## 3. Body ------------------------------

dashbody = dashboardBody(
  tabItems(
    
    # First tab content
    tabItem(tabName = "about",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "timeseries",
            h2("Time Series Analysis",
               fluidRow(
                 br(),
                 tabBox(
                   title = "First tabBox",
                   # The id lets us use input$tabset1 on the server to find the current tab
                   id = "tabset1",
                   height = "1000px",
                   width = "1000px",
                   tabPanel("Tab1", 
                            box(plotOutput("plot1")),
                            
                            box(
                              "Box content here",
                              br(),
                              "More box content",
                              sliderInput("slider", "Slider input:", 1, 100, 50),
                              textInput("text", "Text input:"))
                            ),
                   tabPanel("Tab2", "Tab content 2")
                 )
               ))), 
    # Third tab content
    tabItem(tabName = "snapshot",
            h2("Widgets tab content")
    ),
    # Fourth tab content
    tabItem(tabName = "share",
            h2("Widgets tab content")
    )
  )
)

## Putting UI together --------------------
ui <- dashboardPage(
  header,
  sidebar,
  dashbody
)