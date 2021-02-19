
server <- function(input, output) {

#Time series graphs --------------------
  
#Bar graph of market size by neighborhood_group over time
  
#Organizing the data  
  
  market_size_years = airbnb_data %>% 
    group_by(year, neighbourhood_group) %>% 
    summarize(market_size = sum(availability_by_price))
  
#Plotting the graph
  
  output$market_size_byyears = renderPlot({
    ggplot(market_size_years, aes(x = year, y = market_size)) +
      geom_col(aes(fill = reorder(neighbourhood_group, market_size))) + 
      theme(axis.text.x = element_text(angle=0, vjust=0.6),
            panel.background = element_blank(),
            text = element_text(size = 20 ),
            legend.text=element_text(size=22)) +
      labs(title="Market Size (Supply) Over Time", 
           subtitle="Divided by Neighbourhoods",
           y = 'Market Size',
           x = 'Year' ,
           fill = "Neighbourhood Group") +
      scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
      scale_fill_brewer(palette="Reds") #Colors for neighbourhood group
  })

#Line graph of average availability, average price, and number of listings
#over time (Maybe do facet wrap??)

#Organizing the data  
  
  data_frame0 = airbnb_data %>% 
    group_by(year) %>% 
    summarize(avg_avail = mean(availability_365),
              avg_price = mean(price),
              listings = n())
  
#Plotting the graph
  
  output$var_marketsize_byyears = renderPlot({
  ggplot(data_frame0, aes(x = year)) +
    geom_line(aes(y = avg_avail, group = 1, colour = 'Avg. Availability'),
              size = 3)+
    geom_line(aes(y = avg_price, group = 1, colour = 'Avg. Price'),
              size = 3)+
    geom_line(aes(y = (listings/200), group = 1, colour = 'Listings'),
              size = 3)+
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
    theme(axis.text.x = element_text(angle=0, vjust=0.6),
          panel.background = element_blank(),
          text = element_text(size = 20 ),
          legend.text=element_text(size=22)) +
    scale_color_manual(values = c("Avg. Availability" = "gold1",
                                  "Avg. Price" = "orange1",
                                  "Listings" = "red2"))
  })
  
#Bar graph of total reviews by neighborhood_group over time
  
#Organizing the data
  
  data_frame1 = airbnb_data %>% 
    group_by(year, neighbourhood_group) %>% 
    summarise(reviews_that_year = sum(reviews_this_year))
  
#Plotting the graph
  
  output$reviews_byneigh_byyear = renderPlot({
  ggplot(data_frame1, aes(x = year, y = reviews_that_year)) +
    geom_bar(stat = 'identity',
             aes(fill = reorder(neighbourhood_group, reviews_that_year))) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
    labs(title="Number of Reviews (Demand) Over Time", 
         subtitle="Divided by Neighbourhoods",
         y = 'Reviews',
         x = 'Year' ,
         fill = "Neighbourhood Group") +
    theme(axis.text.x = element_text(angle=0, vjust=0.6),
          panel.background = element_blank(),
          text = element_text(size = 20 ),
          legend.text=element_text(size=22)) +
    scale_y_continuous(labels = scales::unit_format(unit = "k", scale = 1e-3)) +
    scale_fill_brewer(palette="Reds") #Colors for neighbourhood group
  })
  
#Line graph of avg Reviews per Listing Days total and per neighbourhood_group
#over time
  
#Organizing the data
  
  data_frame3 = airbnb_data %>% 
    group_by(neighbourhood_group, year) %>%
    summarize(total_listingdays = sum(availability_365),
              total_reviews = sum(reviews_this_year),
              reviews_per_listingdays = total_reviews/total_listingdays) %>% 
    arrange(desc(reviews_per_listingdays))
  
#Plotting the graph
  
  output$RPLD_perneigh_byyear = renderPlot({
  ggplot(data_frame3, aes(x = year, y = reviews_per_listingdays,
                          group = neighbourhood_group,
                          color = neighbourhood_group)) +
    geom_line(size = 2) +
    scale_color_brewer(palette = "Set1") +
    theme(axis.text.x = element_text(angle=0, vjust=0.6),
          panel.background = element_blank(),
          text = element_text(size = 20 ),
          legend.text=element_text(size=22)) +
    labs(title = "Reviews per Listing Days Ratio",
         subtitle = 'Over Time by Neighbourhood Group',
         y = 'Reviews Per Listing Days',
         x = 'Year',
         color = 'Neighbourhood')
  })
  
#Line graph of average price total and per neighborhood_group over time
  
#Organizing the data
  
  data_frame4 = airbnb_data %>% 
    group_by(year, neighbourhood_group) %>% 
    summarize(avg_price = weighted.mean(price, availability_365)) #weighted by the
  #available days in the year.
  
#Plotting the graph
  
  output$price_byneigh_byyear = renderPlot({
  ggplot(data_frame4, aes(x = year, y = avg_price,
                          group = neighbourhood_group,
                          color = neighbourhood_group)) +
    geom_line(size = 2) +
    scale_color_brewer(palette = "Set1") +
    theme(axis.text.x = element_text(angle=0, vjust=0.5),
          panel.background = element_blank(),
          text = element_text(size = 20 ),
          legend.text=element_text(size=22)) +
    labs(title = "Average Price per Night",
         subtitle = 'Over Time by Neighbourhood Group',
         y = 'Avg. Price',
         x = 'Year',
         color = 'Neighbourhood')
  })
  
#Bar graph filled of market size composition by room_type over time
  
#Organizing the data
  
  data_frame2 = airbnb_data %>% 
    group_by(room_type, year) %>% 
    summarize(market_by_roomtype = sum(availability_by_price))
  
#Plotting the graph
  
  output$market_byroomtype_byyears = renderPlot({
  ggplot(data_frame2, aes(x = year, y = market_by_roomtype)) +
    geom_bar(stat = 'identity', position = 'fill',
             aes(fill = reorder(room_type, market_by_roomtype))) + 
    labs(title = "Supply Distribuition by Room Type", 
         y = NULL,
         x = 'Year' ,
         fill = "Room Type") +
    theme(axis.text.x = element_text(angle=0, vjust=0.6),
          panel.background = element_blank(),
          text = element_text(size = 20 ),
          legend.text=element_text(size=22)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette="Spectral") #Colors for neighbourhood group
  })
  
#Snapshot graphs -----------------------
  
#Year for snapshot
  
  year0 = reactive({airbnb_data %>%
      filter(year == input$year)})
  
#Load Leaflet graph
  
  #map = leaflet(data = year0()) %>% addProviderTiles(providers$CartoDB.Voyager)
  
#Output for heatmap of listings--------------
  
  output$heatmap_listings <- renderLeaflet({
    leaflet(data = year0()) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addHeatmap( #Adds a heatmap
        lng =  year0()$longitude,
        lat =  year0()$latitude,
        blur = 4,
        intensity = NULL,
        cellSize = 1,
        radius = 1
      )  %>%
      addMarkers( #Adds markets in clusters. Have this as an option in Shiny
        lng = year0()$longitude,
        lat = year0()$latitude,
        label = year0()$host_id,
        clusterOptions = markerClusterOptions()
      )
  })
}