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
    
    # First tab content-----------
    tabItem(tabName = "about",
            fluidRow(
            )
    ), 
    
    # Second tab content-----------
    tabItem(tabName = "timeseries",
            h2("Time Series Analysis"),
            box(width = "1000px",
                h3("Market size is calculated by multiplying the number
                   of listings by the available days per year and
                   by the price for each listing and then
                   adding the results."),
                br(),
                plotOutput("market_size_byyears")),
            br(),
            box(width = "1000px",
                h3("Comparison of variables that affect Market Size,
                   all of them are is positively correlated to the Market Size.
                   \'Avg. Availability\' is how many days out of the year
                   the listing was available."),
                br(),
                plotOutput("var_marketsize_byyears")),
            br(),
            box(width = "1000px",
                h3("Number of reviews left by year, which is a proxy
                   for the demand for Airbnb rentals."),
                br(),
                plotOutput("reviews_byneigh_byyear")),
            br(),
            box(width = "1000px",
                h3("This shows the ratio of number of reviews per 
                   listing days available. The higher the ratio the
                   larger the demand for the available supply."),
                br(),
                plotOutput("RPLD_perneigh_byyear")),
            br(),
            box(width = "1000px",
                h3("We can see that the larger relative fall of
                   demand in 2020 has lowered by over 20% 
                   the prices per night in Manhattan."),
                br(),
                plotOutput("price_byneigh_byyear")),
            br(),
            box(width = "1000px",
                h3("This shows the supply, listing days by price
                   per night, for each room type."),
                br(),
                plotOutput("market_byroomtype_byyears"))
            ), 
    # Third tab content-------------
    tabItem(tabName = "snapshot",
            h2("Yearly Snapshot"),
            #User input to select the year for the snapshot
            column(2,
                   selectInput(
                     inputId = "year",
                     label = h3("Select a Year"),
                     choices = c(
                       2020,
                       2019,
                       2018,
                       2017,
                       2016,
                       2015
                     ),
                     selected = 1
                   )),
            br(), br(), br(), br(), br(),
            fluidRow(
              br(),
              tabBox(
                title = "First tabBox",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",
                height = "5000px",
                width = "1000px",
                tabPanel("Heatmaps", 
                         h3("Heatmap of Number of Listings"),
                         br(),
                         box(leafletOutput("heatmap_listings"),
                             width = "1000px"),
                         br(),
                         h3("Heatmap of Prices"),
                         br(),
                         box(leafletOutput("lol"),
                             width = "1000px")
                ),
                tabPanel("Tab2", "Tab content 2")
              )
            )
    ),
    # Fourth tab content--------------
    tabItem(tabName = "share",
            h2("Widgets tab content")
    )
  )
)

##4. Putting UI together --------------------
ui <- dashboardPage(
  header,
  sidebar,
  dashbody
)