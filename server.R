
server <- function(input, output) {
  
  map = leaflet() %>% addProviderTiles(providers$CartoDB.Voyager)
  
  year0 = airbnb_data %>% 
    filter(year == input$year)
  
  output$heatmap_listings <- renderLeaflet({map  %>%
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
  })
}