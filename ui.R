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
                          titleWidth = 230,
                          disable = FALSE)
header$children[[2]]$children <-  tags$a(href='http://mycompanyishere.com',
                                         tags$img(src='logo.png',height='60',width='200'))

## 2. Siderbar ------------------------------

sidebar = dashboardSidebar(
  width = 170,
  sidebarMenu( 
    menuItem("About", tabName = "about", icon = icon("info-circle")),
    menuItem("Time series", tabName = "timeseries", icon = icon("chart-line")),
    menuItem("Snapshop", tabName = "snapshot", icon = icon("chart-bar")),
    menuItem("Data", tabName = "data", icon = icon("database"))
    
  )
)

## 3. Body ------------------------------

dashbody = dashboardBody(
  
  tabItems(
    
    # About tab-----------
    tabItem(tabName = "about",
            fluidPage(
            h2("About this project:"),
            h3("The objective is to understand the Airbnb market in NYC in order to make educated investment decisions.
            \nSome of the key questions being asked are:"),
            br(),
            tags$ol(style = "font-size:20px;",
              tags$li("How has the market evolved over time, as this is a novel business model, how have characteristics in the offering changed?"), 
              br(),
              tags$li("What has happened to the supply and demand side of the equation in this market and what are the underlying factors impacting those changes?"), 
              br(),
              tags$li("What has been the impact of COVID-19 in the market, who has benefited and where is the impact more pronounced?"),
              br(),
              tags$li("How is the market composed in terms of size, price, demand, supply and other characteristics?"),
              br(),
              tags$li("Where are the most interesting investment opportunities with regards to the largest mismatch between supply and demand that could lead to higher occupancy rates and price per night increases?")
            ),
            h2("About the data:"),
            HTML("<h3><p>The data is sourced from <a href='//insideairbnb.com' target='_blank'>insideairbnb.com</a>, an independent non-commercial project that gathers publicly available Airbnb information.</p></h1>"),
            h2("About the creator:"),
            HTML("<h3><p>This data exploration and visualization dashboard has been created by <a href='https://www.linkedin.com/in/daniel-ellenbogen-2130a021/' target='_blank'>Daniel Ellenbogen</a>. I'm passionate about data and its potential impacts so please feel free to connect with me in <a href='https://www.linkedin.com/in/daniel-ellenbogen-2130a021/' target='_blank'>Linkedin</a> to discuss any topic related to that or in case you want to be able to stay in touch for future discussions.</p></h1>"),
            h2("Further analysis:"),
            h3("Some further research that could bring addition insights are:"),
            br(),
            tags$ol(style = "font-size:20px;",
                    tags$li("Analysis of individual reviews to understand the factors that impact the value perceived by the customers in order to tailor offerings that generate the most value."), 
                    br(),
                    tags$li("Analysis of the elasticity of the price in the market to determine if there are a subset of assets where value could be generated through acquisition and optimization of the pricing strategy."), 
                    br(),
                    tags$li("Combining this data with available real estate data on land value to determine if this data can help predict increases in value in the real estate market."),
                    br(),
                    tags$li("Comparing this data with hotel information to gather insights into how the traditional hospitality industry could better compete by tailoring its offer to the needs of Airbnb customers."),
            )
            )
    ), 
    
    # Timeseries tab-----------
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
    #Snapshot tab-------------
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
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",
                height = "5000px",
                width = "1000px",
                # General info boxes---------
                tabPanel("General Info",
                         br(),
                         valueBoxOutput("listings"),
                         valueBoxOutput("listing_days"),
                         valueBoxOutput("avg_price"),
                         valueBoxOutput("avg_avail"),
                         valueBoxOutput("reviews"),
                         valueBoxOutput("reviews_listingdays")),
                
                # Heatmaps--------------
                tabPanel("Heatmaps", 
                         h3("Heatmap of Number of Listings"),
                         br(),
                         box(leafletOutput("heatmap_listings"),
                             width = "1000px"),
                         br(),
                         h3("Heatmap of Price per Night"),
                         br(),
                         box(leafletOutput("heatmap_price"),
                             width = "1000px"),
                         br(),
                         h3("Heatmap of Number of Reviews"),
                         br(),
                         box(leafletOutput("heatmap_reviews"),
                             width = "1000px")
                ),
                
                # Treemaps-------------
                tabPanel("Treemaps",
                         h3("The Area is the Number Listings and Heatmap are Prices"),
                         br(),
                         box(plotOutput("treemap1"),
                             width = "1000px"),
                         h3("The Area is the Market Size and Heatmap are Prices"),
                         br(),
                         box(plotOutput("treemap2"),
                             width = "1000px"),
                         h3("The Area is the Market Size and Heatmap is Reviews per Listing Days"),
                         br(),
                         box(plotOutput("treemap3"),
                             width = "1000px")
                ),
                
                #Dot Plot--------------
                tabPanel("Dot Plot",
                         box(plotOutput("topN_neight")),
                         
                         box(
                           h3("Select Values to Plot"),
                           h4("Use the slider to select how many how 
                              many of the top neighbourhoods by reviews
                              per listing days ratio to show."),
                           sliderInput("topN_neigh", label = NULL, 5, 25, 15),
                           h4("Enter the minimum market size of the
                              neighbourhood to be considered in the graph."),
                           numericInput("market_size",
                                        h3("In USD$ Millions"),
                                        value = 1,
                                        min = 0,
                                        step = 0.1,
                                        ),
                           #Alters the size of numericInput box and text using CSS
                           tags$head(tags$style(HTML('#market_size{height: 50px}
                            #market_size{font-size: 22px}')))
                         )
                )
              )
            )
    ),
    #Raw data tab--------------
    tabItem(tabName = "data",
            h2("Choose Year to View or Download Data"),
            selectInput("dataset", NULL,
                        choices = c("All Years",
                                    2020,
                                    2019,
                                    2017,
                                    2016,
                                    2015)),
            downloadButton("downloadData", "Download"),
            tableOutput("table")
    )
  )
)

##4. Putting UI together --------------------
ui <- dashboardPage(
  skin = "red",
  header,
  sidebar,
  dashbody
)