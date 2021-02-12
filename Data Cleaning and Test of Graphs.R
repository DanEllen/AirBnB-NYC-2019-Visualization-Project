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

##Load workspace

load ("C:/Users/Daniel/Dropbox/Data Science/NYC DSA/R/Exploratory Visualization and Shiny Project/AirBnB-NYC-2019-Visualization-Project/.RData")

#Loading AirBnb 2015-2020 Data into a single dataframe and adding a column
#to identify which year the observation belongs to
setwd("C:/Users/Daniel/Dropbox/Data Science/NYC DSA/R/Exploratory Visualization and Shiny Project/AirBnB-NYC-2019-Visualization-Project")

airbnb_data = purrr::map_dfr(list.files(pattern="*.csv", full.names = TRUE),
               ~read.csv(.x) %>% mutate(year = sub(".csv$", "", basename(.x)),
                                        year = sub("listings ", "", year),
                                        year = as.factor(year)))

#Understanding the data
names(airbnb_data)
str(airbnb_data)
summary(airbnb_data)
colMeans(is.na(airbnb_data)) #Find the percetage of NA points in all columns

ggplot(airbnb_data, aes(y = number_of_reviews)) +
  geom_boxplot() + 
  coord_cartesian(ylim = c(0, 20))

sum(airbnb_data$number_of_reviews == '0')/length(airbnb_data$number_of_reviews)

summary(airbnb_data$number_of_reviews)

#Getting a data frame for each year to use in graphs as needed

airbnb_data2015 = airbnb_data %>% 
  filter(year == '2015')

airbnb_data2016 = airbnb_data %>% 
  filter(year == '2016')

airbnb_data2017 = airbnb_data %>% 
  filter(year == '2017')

airbnb_data2018 = airbnb_data %>% 
  filter(year == '2018')

airbnb_data2019 = airbnb_data %>% 
  filter(year == '2019')

airbnb_data2020 = airbnb_data %>% 
  filter(year == '2020')


#Getting the reviews for that year by substracting the reviews the past year.
#Setting reviews to 0 when the number comes out negative 9 which dosent make
#sense and happens in less than 0.3% of cases. 

airbnb_data2020$reviews_this_year = 
  airbnb_data2020$number_of_reviews - 
  airbnb_data2019$number_of_reviews[match
                                    (airbnb_data2020$id, airbnb_data2019$id)] 

airbnb_data2020$reviews_this_year =
  ifelse(is.na(airbnb_data2020$reviews_this_year),
         airbnb_data2020$number_of_reviews,
         airbnb_data2020$reviews_this_year)

airbnb_data2020$reviews_this_year =
  ifelse(airbnb_data2020$reviews_this_year < 0,
         0,
         airbnb_data2020$reviews_this_year)

summary(airbnb_data2020$reviews_this_year)

#Creating column to calculate market size by multiplying available days by
#average price.

market_size_func = function(dataframe) {
  dataframe %>% 
    mutate(availability_by_price = availability_365 * price)
}

airbnb_data = market_size_func(airbnb_data)
airbnb_data2015 = market_size_func(airbnb_data2015)
airbnb_data2016 = market_size_func(airbnb_data2016)
airbnb_data2017 = market_size_func(airbnb_data2017)
airbnb_data2018 = market_size_func(airbnb_data2018)
airbnb_data2019 = market_size_func(airbnb_data2019)
airbnb_data2020 = market_size_func(airbnb_data2020)

##############################################
#1 Map with options to see density (heat map) based number of listings, 
#price per night, and reviews per month. Putting a max value on the price
#per night as there are some outliers that will skew the visualization. 

#Base map

map = leaflet() %>% addProviderTiles(providers$CartoDB.Voyager)

#Heatmap optimized for number of listings

map  %>%
  addHeatmap( #Adds a heatmap
    lng =  airbnb_data2020$longitude,
    lat =  airbnb_data2020$latitude,
    blur = 4,
    intensity = NULL,
    cellSize = 1,
    radius = 1
  )  %>%
  addMarkers( #Adds markets in clusters. Have this as an option in Shiny
    lng = airbnb_data2020$longitude,
    lat = airbnb_data2020$latitude,
    label = airbnb_data2020$host_id,
    clusterOptions = markerClusterOptions()
  )
as.vector(airbnb_data2020$price)

#Heatmap optimized for price of listings

quant_98_price_max = as.numeric(quantile(airbnb_data2020$price, c(0.98)))

map  %>%
  addHeatmap( #Adds a heatmap
    lng =  airbnb_data2020$longitude,
    lat =  airbnb_data2020$latitude,
    intensity = airbnb_data2020$price,
    blur = 2,
    max = quant_98_price_max,
    cellSize = 3,
    radius = 2
  )

#Heatmap optimized for price of listings

quant_95_review_max = as.numeric(quantile(airbnb_data2020$reviews_this_year, c(0.95), na.rm = T))

map  %>%
  addHeatmap( #Adds a heatmap
    lng =  airbnb_data2020$longitude,
    lat =  airbnb_data2020$latitude,
    intensity = airbnb_data2020$reviews_this_year,
    blur = 2,
    max = quant_95_review_max,
    cellSize = 2,
    radius = 2
  )

##############################################
#2.	Graphs for composition by neighborhood comparing number of listings, 
#price per night, and reviews per month. 

#Treemap graph for number of listing comparing neighborhood_group and
#neighboorhood and price per night

#Organize the data

treemap_data1 = airbnb_data2020 %>% 
  group_by(neighbourhood, neighbourhood_group) %>% 
  summarize(listing_days = sum(availability_365),
            avg_price = mean(price))

#Plot the graph

ggplot(treemap_data1, aes(area = listing_days, fill = avg_price,
                          label = neighbourhood,
                          subgroup = neighbourhood_group)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.6, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "grey81", place = "topleft", reflow = T) +
  scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint = 170,
                       limits = c(80, 400), oob = scales::squish) +
  labs(fill = "Price per Night")

#Treemap graph for market size comparing neighborhood_group and
#neighboorhood and price per night

#Organize the data

treemap_data2 = airbnb_data2020 %>% 
  group_by(neighbourhood, neighbourhood_group) %>% 
  summarize(market_size = sum(availability_by_price),
            avg_price = mean(price))

#Plot the graph

ggplot(treemap_data2, aes(area = market_size, fill = avg_price,
                         label = neighbourhood,
                         subgroup = neighbourhood_group)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.6, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "grey81", place = "topleft", reflow = T) +
  scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint = 170,
                       limits = c(80, 400), oob = scales::squish) +
  labs(fill = "Price per Night")

#Treemap graph for size of offering (listing by days available by price)
#comparing neighborhood_group, neighboorhood, and avg reviews per month

#Organize the data

treemap_data3 = airbnb_data2020 %>% 
  group_by(neighbourhood, neighbourhood_group) %>% 
  summarize(market_size = sum(availability_by_price),
            total_listingdays = sum(availability_365),
            total_reviews = sum(reviews_this_year),
            reviews_per_listingdays = total_reviews/total_listingdays)

#Plot the graph

ggplot(treemap_data3, aes(area = market_size,
                          fill = reviews_per_listingdays,
                          label = neighbourhood,
                          subgroup = neighbourhood_group)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.6, colour =
                               "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "grey81", place = "topleft", reflow = T) +
  scale_fill_gradient2(low="white", mid="yellow", high="red", 
                       #midpoint = 0.04,
                       limits = c(0, 0.1),
                       oob = scales::squish) +
  labs(fill = "RPLD",
       title = 'Market Size by Neighbourhood \nand Reviews per Listing Days Heatmap') +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5))


#Dot plot of neighbourhoods and RPLD ratio for top N neigh... and point size
#based on market size. Point label shows market size in $ and % of total.

#Creating the table needed for the scatterplot

total_market_size_2020 = sum(airbnb_data2020$availability_by_price)
market_size_filter = 1000000

scatter_plot_data = airbnb_data2020 %>% 
  group_by(neighbourhood) %>%
  summarize(market_size = sum(availability_by_price),
            total_listingdays = sum(availability_365),
            total_reviews = sum(reviews_this_year),
            reviews_per_listingdays = total_reviews/total_listingdays,
            market_share = market_size / total_market_size_2020) %>% 
  mutate(market_share = as.numeric(format(round(market_share, 4), nsmall = 2))) %>% 
  filter(market_size>market_size_filter) %>%
  top_n(n = 15, wt = reviews_per_listingdays) %>% 
  arrange(desc(reviews_per_listingdays))

#Graphing

ggplot(scatter_plot_data, aes(x = reorder(neighbourhood, reviews_per_listingdays) ,
                              y = reviews_per_listingdays,
                              label = scales::percent(market_share),
                              size = market_size)) + 
  geom_point(col="tomato2")  +
  geom_segment(aes(x = neighbourhood, 
                   xend = neighbourhood, 
                   y = min(reviews_per_listingdays), 
                   yend = max(reviews_per_listingdays)), 
               linetype = "dashed", 
               size = 0.1,
               alpha = 0.2) +
  #ylim(-2.5, 2.5) +
  labs(title="Ratio of Reviews per Listing Days", 
       subtitle="Top 15 Neighbourhoods",
       y = "Reviews per Listing Days",
       size = "Market Size") +  
  theme_classic() +
  theme(axis.title.y = element_blank()) +
  scale_size(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  coord_flip()

#Bar graph of market size by neighborhood_group over time

#Getting the dataframe for the graph

market_size_years = airbnb_data %>% 
  group_by(year, neighbourhood_group) %>% 
  summarize(market_size = sum(availability_by_price))

#Graph

ggplot(market_size_years, aes(x = year, y = market_size)) +
  geom_bar(stat = 'identity',
           aes(fill = reorder(neighbourhood_group, market_size))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Market Size Over Time", 
       subtitle="Divided by Neighbourhoods",
       y = 'Market Size',
       x = 'Year' ,
       fill = "Neighbourhood Group") +
  theme_classic() +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_brewer(palette="Reds") #Colors for neighbourhood group

#Line graph of average availability, average price, and number of listings
#over time (Maybe do facet wrap??)

#Bar graph of number of reviews over time. Comment correlation between listing
#days and number of reviews

cor(treemap_data3$total_listingdays, treemap_data3$total_reviews)

#Bar graph filled of market size composition by room_type over time

#Line graph of avg Reviews per Listing Days total and per neighbourhood_group
#over time

#Line graph of average price total and per neighborhood_group over time