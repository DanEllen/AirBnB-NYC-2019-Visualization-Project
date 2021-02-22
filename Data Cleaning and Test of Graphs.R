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

load('airbnb_data')

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

#Getting the reviews for that year by subtracting the reviews the past year.
#If there is no ID for that property last year it keeps the reviews (we assume
#its the first year of operation for that property).
#Setting reviews to 0 when the number comes out negative 9 which doesn't make
#sense and happens in less than 0.3% of cases in 2020. 

get_reviews = function(df1, df2) {
  ### Get the number of reviews this year by susbtracting last years
  ### when there is no ID last year it just keeps the number of reviews.
  ### If the number comes negative it makes it 0, it happens in very few cases.
  
  df1$reviews_this_year = 
    df1$number_of_reviews - 
    df2$number_of_reviews[match(df1$id, df2$id)] 
  
  df1$reviews_this_year =
    ifelse(is.na(df1$reviews_this_year),
           df1$number_of_reviews,
           df1$reviews_this_year)
  
  df1$reviews_this_year =
    ifelse(df1$reviews_this_year < 0,
           0,
           df1$reviews_this_year)
  return(df1$reviews_this_year)
}

airbnb_data2020$reviews_this_year = get_reviews(airbnb_data2020, airbnb_data2019)
airbnb_data2019$reviews_this_year = get_reviews(airbnb_data2019, airbnb_data2018)
airbnb_data2018$reviews_this_year = get_reviews(airbnb_data2018, airbnb_data2017)
airbnb_data2017$reviews_this_year = get_reviews(airbnb_data2017, airbnb_data2016)
airbnb_data2016$reviews_this_year = get_reviews(airbnb_data2016, airbnb_data2015)
airbnb_data2015$reviews_this_year = airbnb_data2015$number_of_reviews


#Creating one dataframe that contains all the yearly data

airbnb_data = do.call("rbind", list(airbnb_data2020,
                                    airbnb_data2019,
                                    airbnb_data2018,
                                    airbnb_data2017,
                                    airbnb_data2016,
                                    airbnb_data2015))

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

#Save dataframes that will be used in Shiny App.

save(airbnb_data, file = 'airbnb_data')
save(airbnb_data2020, file = 'airbnb_data2020')
save(airbnb_data2019, file = 'airbnb_data2019')
save(airbnb_data2018, file = 'airbnb_data2018')
save(airbnb_data2017, file = 'airbnb_data2017')
save(airbnb_data2016, file = 'airbnb_data2016')
save(airbnb_data2015, file = 'airbnb_data2015')

##############################################
#1 Map with options to see density (heat map) based number of listings, 
#price per night, and reviews per month. Putting a max value on the price
#per night as there are some outliers that will skew the visualization. 

#Base map

map = leaflet() %>% addProviderTiles(providers$CartoDB.Voyager)

#Heatmap optimized for number of listings

year0 = airbnb_data %>% 
  filter(., year == 2020)

map  %>%
  addHeatmap( #Adds a heatmap
    lng =  year0$longitude,
    lat =  year0$latitude,
    blur = 4,
    intensity = NULL,
    cellSize = 1,
    radius = 1
  )  %>%
  addMarkers( #Adds markets in clusters. Have this as an option in Shiny
    lng = year0$longitude,
    lat = year0$latitude,
    label = year0$host_id,
    clusterOptions = markerClusterOptions()
  )

#Heatmap optimized for price of listings

quant_98_price_max = as.numeric(quantile(year0$price, c(0.98)))

map  %>%
  addHeatmap( #Adds a heatmap
    lng =  year0$longitude,
    lat =  year0$latitude,
    intensity = year0$price,
    blur = 4,
    max = quant_98_price_max,
    cellSize = 1,
    radius = 1
  )

#Heatmap optimized for reviews

quant_95_review_max = as.numeric(quantile(year0$reviews_this_year, c(0.95), na.rm = T))

map  %>%
  addHeatmap( #Adds a heatmap
    lng =  year0$longitude,
    lat =  year0$latitude,
    intensity = year0$reviews_this_year,
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

treemap_data1 = year0 %>% 
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
  labs(fill = "Price per Night",
       title = 'Number of Listings by Neighbourhood\nand Price Heatmap')

#Treemap graph for market size comparing neighborhood_group and
#neighboorhood and price per night

#Organize the data

treemap_data2 = year0 %>% 
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
  labs(fill = "Price per Night",
       title = 'Market Size by Neighbourhood\nand Price Heatmap')

#Treemap graph for size of offering (listing by days available by price)
#comparing neighborhood_group, neighboorhood, and avg reviews per month

#Organize the data

treemap_data3 = year0 %>% 
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
       title = 'Market Size by Neighbourhood and\nReviews per Listing Days Heatmap') +
  theme(legend.position="none", plot.title = element_text(hjust = 0.0))


#Dot plot of neighbourhoods and RPLD ratio for top N neigh... and point size
#based on market size. Point label shows market size in $ and % of total.

#Creating the table needed for the scatterplot

total_market_size = sum(year0$availability_by_price)
market_size_filter = 1000000

dot_plot_data = year0 %>% 
  group_by(neighbourhood) %>%
  summarize(market_size = sum(availability_by_price),
            total_listingdays = sum(availability_365),
            total_reviews = sum(reviews_this_year),
            reviews_per_listingdays = total_reviews/total_listingdays,
            market_share = market_size / total_market_size) %>% 
  mutate(market_share = as.numeric(format(round(market_share, 4), nsmall = 2))) %>% 
  filter(market_size>market_size_filter) %>%
  top_n(n = 15, wt = reviews_per_listingdays) %>% 
  arrange(desc(reviews_per_listingdays))

#Graphing

ggplot(dot_plot_data, aes(x = reorder(neighbourhood, reviews_per_listingdays) ,
                              y = reviews_per_listingdays,
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
  geom_col(aes(fill = reorder(neighbourhood_group, market_size))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        panel.background = element_blank(),
        text = element_text(size = 12 )) +
  labs(title="Market Size (Supply) Over Time", 
       subtitle="Divided by Neighbourhoods",
       y = 'Market Size',
       x = 'Year' ,
       fill = "Neighbourhood Group") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_brewer(palette="Reds") #Colors for neighbourhood group

#Line graph of average availability, average price, and number of listings
#over time (Maybe do facet wrap??)

data_frame0 = airbnb_data %>% 
  group_by(year) %>% 
  summarize(avg_avail = mean(availability_365),
            avg_price = weighted.mean(price, availability_365),
            listings = n())

ggplot(data_frame0, aes(x = year)) +
  geom_line(aes(y = avg_avail, group = 1, colour = 'Avg. Availability'),
            size = 1.5)+
  geom_line(aes(y = avg_price, group = 1, colour = 'Avg. Price'),
            size = 1.5)+
  geom_line(aes(y = (listings/200), group = 1, colour = 'Listings'),
            size = 1.5)+
  scale_y_continuous(name = "Price and Availability",
                     sec.axis = sec_axis(~.*200, name="Listings",
                                         labels = scales::unit_format(unit = "k", scale = 1e-3),
                                         breaks = seq(0,55000, by = 5000)),
                     limits=c(100,260),
                     breaks = seq(0, 250, by = 25),) +
  labs(title="Variables of Market Size",
       subtitle = "Evolution over Years",
       x = 'Year',
       colour = "Legend") +
  scale_color_manual(values = c("Avg. Availability" = "gold1",
                                "Avg. Price" = "orange1",
                                "Listings" = "red2")) +
  theme_classic() 

#Bar graph of total reviews by neighborhood_group over time

#Organizing the data

data_frame1 = airbnb_data %>% 
  group_by(year, neighbourhood_group) %>% 
  summarise(reviews_that_year = sum(reviews_this_year))

#Ploting the graph

ggplot(data_frame1, aes(x = year, y = reviews_that_year)) +
  geom_bar(stat = 'identity',
           aes(fill = reorder(neighbourhood_group, reviews_that_year))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Number of Reviews (Demand) Over Time", 
       subtitle="Divided by Neighbourhoods",
       y = 'Reviews',
       x = 'Year' ,
       fill = "Neighbourhood Group") +
  theme_classic() +
  scale_y_continuous(labels = scales::unit_format(unit = "k", scale = 1e-3)) +
  scale_fill_brewer(palette="Reds") #Colors for neighbourhood group

#Bar graph filled of market size composition by room_type over time

#Organizing the data

data_frame2 = airbnb_data %>% 
  group_by(room_type, year) %>% 
  summarize(market_by_roomtype = sum(availability_by_price))

#Ploting the graph

ggplot(data_frame2, aes(x = year, y = market_by_roomtype)) +
  geom_bar(stat = 'identity', position = 'fill',
           aes(fill = reorder(room_type, market_by_roomtype))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title = "Supply Distribuition by Room Type", 
       y = NULL,
       x = 'Year' ,
       fill = "Room Type") +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette="Spectral") #Colors for neighbourhood group

#Line graph of avg Reviews per Listing Days total and per neighbourhood_group
#over time

#Order data

data_frame3 = airbnb_data %>% 
  group_by(neighbourhood_group, year) %>%
  summarize(total_listingdays = sum(availability_365),
            total_reviews = sum(reviews_this_year),
            reviews_per_listingdays = total_reviews/total_listingdays) %>% 
  arrange(desc(reviews_per_listingdays))

#Graph

ggplot(data_frame3, aes(x = year, y = reviews_per_listingdays,
           group = neighbourhood_group,
           color = neighbourhood_group)) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Reviews per Listing Days Ratio",
       subtitle = 'Over Time by Neighbourhood Group',
       y = 'Reviews Per Listing Days',
       x = 'Year',
       color = 'Neighbourhood')+
  theme_classic()

#Line graph of average price total and per neighborhood_group over time

#Gathering data

data_frame4 = airbnb_data %>% 
  group_by(year, neighbourhood_group) %>% 
  summarize(avg_price = weighted.mean(price, availability_365)) #weighted by the
#available days in the year.

#Graph

ggplot(data_frame4, aes(x = year, y = avg_price,
                        group = neighbourhood_group,
                        color = neighbourhood_group)) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Average Price per Night",
       subtitle = 'Over Time by Neighbourhood Group',
       y = 'Avg. Price',
       x = 'Year',
       color = 'Neighbourhood')+
  theme_classic()


#Calculations------------

#Change in Market size from 2019 to 2020

market_size = airbnb_data %>% 
  filter(year == 2020 | year == 2019) %>% 
  group_by(year) %>% 
  summarize(market_size = sum(availability_by_price))

market_size$market_size[2] / market_size$market_size[1] - 1

#Change in reviews from 2019 to 2020

reviews = airbnb_data %>% 
  filter(year == 2020 | year == 2019) %>% 
  group_by(year) %>% 
  summarize(reviews = sum(reviews_this_year))

reviews$reviews[2] / reviews$reviews[1] - 1

#Change in price per night in Manhattan from 2019 to 2020

price = airbnb_data %>% 
  filter(year == 2020 | year == 2019, neighbourhood_group == "Manhattan") %>% 
  group_by(year, neighbourhood_group) %>% 
  summarize(price = mean(price))

price$price[2] / price$price[1] - 1

#Distribuition in 2020 by room type

room_type = airbnb_data %>% 
  filter(year == 2020) %>% 
  group_by(room_type) %>% 
  summarize(availability_by_price = sum(availability_by_price)) %>% 
  mutate(dist = availability_by_price/sum(availability_by_price))
