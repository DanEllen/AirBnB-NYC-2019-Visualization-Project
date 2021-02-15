library(shiny)
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

function(input, output, session) {
  
  observe({
    dest <- unique(flights %>%
                     filter(flights$origin == input$origin) %>%
                     .$dest)
    updateSelectizeInput(
      session, "dest",
      choices = dest,
      selected = dest[1])
  })
  
  flights_delay <- reactive({
    flights %>%
      filter(origin == input$origin & dest == input$dest) %>%
      group_by(carrier) %>%
      summarise(n = n(),
                departure = mean(dep_delay),
                arrival = mean(arr_delay))
  })

  output$delay <- renderPlot(
    flights_delay() %>%
      gather(key = type, value = delay, departure, arrival) %>%
      ggplot(aes(x = carrier, y = delay, fill = type)) +
      geom_col(position = "dodge") +
      ggtitle("Average delay")
  )

  output$count <- renderPlot(
    flights_delay() %>%
      ggplot(aes(x = carrier, y = n)) +
      geom_col(fill = "lightblue") +
      ggtitle("Number of flights")
  )
}