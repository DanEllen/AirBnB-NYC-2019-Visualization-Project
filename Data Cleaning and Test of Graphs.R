#Loading AirBnb NYC 2019 Data
setwd("C:/Users/Daniel/Dropbox/Data Science/NYC DSA/R/Exploratory Visualization and Shiny Project/AirBnB-NYC-2019-Visualization-Project")
airbnb_data = read.csv(file = './AB_NYC_2019.csv')

#Understanding the data
names(airbnb_data)
str(airbnb_data)
summary(airbnb_data)

##############################################
#About libraries used

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


##############################################
#1 Map with options to see density (heat map) based number of listings, 
#price per night, and reviews per month. 

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
  addMarkers( #Adds markets in clusters. Have this as an option in Shiny
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

##############################################
#2.	Graphs for composition by neighborhood comparing number of listings, 
#price per night, and reviews per month. 

#Treemap graph for number of listing comparing neighborhood_group and
#neighboorhood

#Organize the data

treemap_data = airbnb_data %>% 
  group_by(neighbourhood, neighbourhood_group) %>% 
  summarize(total_listings = n(), avg_price = mean(price))

#Plot the graph

ggplot(treemap_data, aes(area = total_listings, fill = avg_price,
                         label = neighbourhood,
                         subgroup = neighbourhood_group)) +
  geom_treemap() +
  scale_alpha_continuous(range = c(0.2, 1)) +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.6, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "grey78", place = "topleft", reflow = T) +
  scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint = 170,
                       limits = c(80, 400), oob = scales::squish) +
  labs(fill = "Average Price")
  
  

