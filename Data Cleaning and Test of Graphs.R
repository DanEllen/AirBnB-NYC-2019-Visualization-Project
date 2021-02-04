#Loading AirBnb NYC 2019 Data
setwd("C:/Users/Daniel/Dropbox/Data Science/NYC DSA/R/Exploratory Visualization and Shiny Project/AirBnB-NYC-2019-Visualization-Project")
airbnb_data = read.csv(file = './AB_NYC_2019.csv')

#Understanding the data
names(airbnb_data)
str(airbnb_data)
summary(airbnb_data)

#Objective.	Map with options to see density (heat map) based on
#number of listings, price, and reviews per month. 

#Leaflet is one of the most popular open-source JavaScript libraries for 
#interactive maps. This leaflet R package makes it easy to integrate and control
#Leaflet maps in R. 
#leaflet.extras package provides a simple heat map function, addheatmap().



install.packages("leaflet")
install.packages('leaflet.extras')

library(leaflet)
library(leaflet.extras)

#Base map

map = leaflet() %>% addProviderTiles(providers$CartoDB.Voyager)

#Heatmap optimized for number of listings

map  %>%
  addHeatmap( #Adds a heatmap
    lng =  airbnb_data$longitude,
    lat =  airbnb_data$latitude,
    blur = 8,
    intensity = NULL,
    max = 50,
    cellSize = 7,
    radius = 5
  )  %>%
  addMarkers( #Adds markets in clusters
    lng = airbnb_data$longitude,
    lat = airbnb_data$latitude,
    label = airbnb_data$host_id,
    clusterOptions = markerClusterOptions()
  )
as.vector(airbnb_data$price)

#Heatmap optimized for price of listings

quantile(airbnb_data$price, c(0.98))

map  %>%
  addHeatmap( #Adds a heatmap
    lng =  airbnb_data$longitude,
    lat =  airbnb_data$latitude,
    intensity = airbnb_data$price,
    blur = 2,
    max = 550,
    cellSize = 3,
    radius = 2
  )

#Heatmap optimized for price of listings

quantile(airbnb_data$reviews_per_month, c(0.95), na.rm = T)

map  %>%
  addHeatmap( #Adds a heatmap
    lng =  airbnb_data$longitude,
    lat =  airbnb_data$latitude,
    intensity = airbnb_data$reviews_per_month,
    blur = 1,
    max = 4.64,
    cellSize = 2,
    radius = 2
  )
