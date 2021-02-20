
server <- function(input, output) {

#Time series graphs --------------------
  
#Bar graph of market size by neighborhood_group over time----------
  
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
            text = element_text(size = 22 ),
            legend.text=element_text(size=22)) +
      labs(title="Market Size (Supply) Over Time", 
           subtitle="Divided by Neighbourhoods",
           y = 'Market Size',
           x = 'Year' ,
           fill = "Neighbourhood Group") +
      scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
      scale_fill_brewer(palette="Reds") #Colors for neighbourhood group
  })

#Line graph of average availability, average price, and number of listings---------
#over time

#Organizing the data  
  
  data_frame0 = airbnb_data %>% 
    group_by(year) %>% 
    summarize(avg_avail = mean(availability_365),
              avg_price = weighted.mean(price, availability_365),
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
          text = element_text(size = 22 ),
          legend.text=element_text(size=22),
          legend.key = element_blank()) +
    scale_color_manual(values = c("Avg. Availability" = "gold1",
                                  "Avg. Price" = "orange1",
                                  "Listings" = "red2"))
  })
  
#Bar graph of total reviews by neighborhood_group over time-----------
  
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
          text = element_text(size = 22 ),
          legend.text=element_text(size=22)) +
    scale_y_continuous(labels = scales::unit_format(unit = "k", scale = 1e-3)) +
    scale_fill_brewer(palette="Reds") #Colors for neighbourhood group
  })
  
#Line graph of avg Reviews per Listing Days total and per neighbourhood_group------------
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
          text = element_text(size = 22 ),
          legend.text=element_text(size=22),
          legend.key = element_blank()) +
    labs(title = "Reviews per Listing Days Ratio",
         subtitle = 'Over Time by Neighbourhood Group',
         y = 'Reviews Per Listing Days',
         x = 'Year',
         color = 'Neighbourhood')
  })
  
#Line graph of average price total and per neighborhood_group over time--------------
  
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
          text = element_text(size = 22 ),
          legend.text=element_text(size=22),
          legend.key = element_blank()) +
    labs(title = "Average Price per Night",
         subtitle = 'Over Time by Neighbourhood Group',
         y = 'Avg. Price',
         x = 'Year',
         color = 'Neighbourhood')
  })
  
#Bar graph filled of market size composition by room_type over time------------
  
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
          text = element_text(size = 22 ),
          legend.text=element_text(size=22)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette="Reds") #Colors for neighbourhood group
  })
  
#Snapshot graphs -----------------------
  
#Year for snapshot reactive function

  year0 = reactive({airbnb_data %>%
      filter(year == input$year)})
  
#Info for value boxes--------------

#listings box  
  
  listings0 = reactive({
    airbnb_data %>% 
      filter(year == input$year) %>% 
      summarize(listings = n())
  })
  
  output$listings <- renderValueBox({
    valueBox(
      paste0(format(listings0()$listings,big.mark=",",scientific=FALSE), ""), "Listings", icon = icon("list"),
      color = "purple"
    )
  })
  
#listing days box  
  
  listings1 = reactive({
    airbnb_data %>% 
      filter(year == input$year) %>% 
      summarize(listings_days = sum(availability_365))
  })
  
  output$listing_days <- renderValueBox({
    valueBox(
      #paste0(format(listings1()$listings_days,big.mark=",",scientific=FALSE), ""),
      paste0(paste(format(round(listings1()$listings_days / 1e6, 2), trim = TRUE), "Million")),
      "Listings Days",
      icon = icon("calendar-day"),
      color = "purple"
    )
  })
  
#avg_price box  
  
  listings2 = reactive({
    airbnb_data %>% 
      filter(year == input$year) %>% 
      summarize(avg_price = weighted.mean(price, availability_365))
  })
  
  output$avg_price <- renderValueBox({
    valueBox(
      #paste0(format(listings1()$listings_days,big.mark=",",scientific=FALSE), ""),
      paste0(paste(format(round(listings2()$avg_price, 0), trim = TRUE), "$")),
      "Avg. Price per Night",
      icon = icon("hand-holding-usd"),
      color = "purple"
    )
  })
  
#avg_avail box  
  
  listings3 = reactive({
    airbnb_data %>% 
      filter(year == input$year) %>% 
      summarize(avg_avail = mean(availability_365))
  })
  
  output$avg_avail <- renderValueBox({
    valueBox(
      #paste0(format(listings1()$listings_days,big.mark=",",scientific=FALSE), ""),
      paste0(paste(format(round(listings3()$avg_avail, 0), trim = TRUE), "Days")),
      "Avg. Availabilty per Year",
      icon = icon("calendar-check"),
      color = "purple"
    )
  })
  
#reviews box  
  
  listings4 = reactive({
    airbnb_data %>% 
      filter(year == input$year) %>% 
      summarize(reviews = sum(reviews_this_year))
  })
  
  output$reviews <- renderValueBox({
    valueBox(
      #paste0(format(listings1()$listings_days,big.mark=",",scientific=FALSE), ""),
      paste0(paste(format(round(listings4()$reviews/ 1e3, 0), trim = TRUE), "k")),
      "Number of Reviews in Thousands",
      icon = icon("gavel"),
      color = "purple"
    )
  })
  
#reviews_listingdays box  
  
  listings5 = reactive({
    airbnb_data %>% 
      filter(year == input$year) %>% 
      summarize(total_listingdays = sum(availability_365),
                total_reviews = sum(reviews_this_year),
                reviews_listingdays = total_reviews/total_listingdays)
  })
  
  output$reviews_listingdays <- renderValueBox({
    valueBox(
      #paste0(format(listings1()$listings_days,big.mark=",",scientific=FALSE), ""),
      paste0(paste(format(round(listings5()$reviews_listingdays, 3), trim = TRUE), "")),
      "Reviews per Listing Days",
      icon = icon("signal"),
      color = "purple"
    )
  })
  
#Heatmap of listings--------------
  
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

#Heatmap of price per night--------------    
  
  output$heatmap_price <- renderLeaflet({
    leaflet(data = year0()) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addHeatmap(
        #Adds a heatmap
        lng =  year0()$longitude,
        lat =  year0()$latitude,
        intensity = year0()$price,
        blur = 2,
        max = as.numeric(quantile(year0()$price, c(0.98))),
        cellSize = 3,
        radius = 2
    )
  })

#Heatmap of reviews--------------    
  
  output$heatmap_reviews <- renderLeaflet({
    leaflet(data = year0()) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      addHeatmap( #Adds a heatmap
        lng =  year0()$longitude,
        lat =  year0()$latitude,
        intensity = year0()$reviews_this_year,
        blur = 2,
        max = as.numeric(quantile(year0()$reviews_this_year, c(0.95), na.rm = T)),
        cellSize = 2,
        radius = 2
      )
  })  
  
#Treemap graph for number of listing comparing neighborhood_group and --------
#neighboorhood and price per night
  
  #Organize the data
  
  treemap_data1 = reactive({airbnb_data %>%
      filter(year == input$year) %>% 
      group_by(neighbourhood, neighbourhood_group) %>%
      summarize(listing_days = sum(availability_365),
                avg_price = mean(price))
  })
  
  #Plot the graph
  
  output$treemap1 = renderPlot({
  ggplot(treemap_data1(), aes(area = listing_days, fill = avg_price,
                            label = neighbourhood,
                            subgroup = neighbourhood_group)) +
    geom_treemap() +
    geom_treemap_subgroup_border() +
    geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.6, colour =
                                 "black", fontface = "italic", min.size = 0) +
    geom_treemap_text(colour = "grey81", place = "topleft", reflow = T) +
    scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint = 170,
                         limits = c(80, 400), oob = scales::squish) +
    theme(text = element_text(size = 20 ),
          legend.text=element_text(size=22)) +
    labs(fill = "Price $\nper Night")
    })
  
#Treemap graph for market size comparing neighborhood_group and ---------
#neighboorhood and price per night
  
  #Organize the data
  
  treemap_data2 = reactive({airbnb_data %>%
    filter(year == input$year) %>% 
    group_by(neighbourhood, neighbourhood_group) %>% 
    summarize(market_size = sum(availability_by_price),
              avg_price = mean(price))
  })
  
  #Plot the graph
  
  output$treemap2 = renderPlot({
  ggplot(treemap_data2(), aes(area = market_size, fill = avg_price,
                            label = neighbourhood,
                            subgroup = neighbourhood_group)) +
    geom_treemap() +
    geom_treemap_subgroup_border() +
    geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.6, colour =
                                 "black", fontface = "italic", min.size = 0) +
    geom_treemap_text(colour = "grey81", place = "topleft", reflow = T) +
    scale_fill_gradient2(low="white", mid="yellow", high="red", midpoint = 170,
                         limits = c(80, 400), oob = scales::squish) +
    theme(text = element_text(size = 20 ),
          legend.text=element_text(size=22)) +
    labs(fill = "Price $\nper Night")
  })
  
  
#Treemap graph for size of offering (listing by days available by price)-------
#comparing neighborhood_group, neighboorhood, and avg reviews per month
  
  #Organize the data
  
  treemap_data3 = reactive({airbnb_data %>%
    filter(year == input$year) %>% 
    group_by(neighbourhood, neighbourhood_group) %>% 
    summarize(market_size = sum(availability_by_price),
              total_listingdays = sum(availability_365),
              total_reviews = sum(reviews_this_year),
              reviews_per_listingdays = total_reviews/total_listingdays)
  })
  
  #Plot the graph
  
  output$treemap3 = renderPlot({
  ggplot(treemap_data3(), aes(area = market_size,
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
                         limits = c(0, 0.2),
                         oob = scales::squish) +
    labs(fill = "Reviews \nper Listing\nDays") +
    theme(legend.position = "right",
          text = element_text(size = 20),
          plot.title = element_text(hjust = 0.0),
          legend.text = element_text(size=22))
  })
  
#Dot plot of neighbourhoods and RPLD ratio for top N neigh and point size ------
#based on market size.
  
  #Creating the table needed for the scatterplot
  
  market_size_filter = 1000000
  
  
  dot_plot_data = reactive({airbnb_data %>%
    filter(year == input$year) %>% 
    group_by(neighbourhood) %>%
    summarize(market_size = sum(availability_by_price),
              total_listingdays = sum(availability_365),
              total_reviews = sum(reviews_this_year),
              reviews_per_listingdays = total_reviews/total_listingdays) %>% 
    filter(market_size>(input$market_size*1e6)) %>%
    top_n(n = input$topN_neigh, wt = reviews_per_listingdays) %>% 
    arrange(desc(reviews_per_listingdays))
  })
  
  #Graphing
  
  output$topN_neight = renderPlot({
  ggplot(dot_plot_data(), aes(x = reorder(neighbourhood, reviews_per_listingdays) ,
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
    theme(axis.title.y = element_blank(),
          legend.position = "right",
          text = element_text(size = 20),
          legend.text = element_text(size=20),
          panel.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank()) +
    scale_size(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
    coord_flip()
  })
  
#Data---------------
  
  datasetInput <- reactive({
    airbnb_data %>% 
      filter(year == input$dataset | input$dataset == "All Years") %>% 
      group_by(neighbourhood, neighbourhood_group) %>% 
      summarize(number_of_listings = n(),
                reviews = sum(reviews_this_year),
                avg_availability = mean(availability_365),
                avg_price = weighted.mean(price, availability_365))
  })

  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = TRUE)
    }
  )

}

