library(leaflet)

# 1. Prepare base geographic data (50 States + DC)
us_centers <- data.frame(state = state.name, lon = state.center$x, lat = state.center$y)
dc_center  <- data.frame(state = "District of Columbia", lon = -77.0163, lat = 38.9047)
all_centers <- rbind(us_centers, dc_center)

# 2. Add your estimated murder rates (Using mock data per 100k people for illustration)
# Replace this column with your actual data frame's column!
set.seed(42)
all_centers$murder_rate <- runif(nrow(all_centers), min = 1.5, max = 12.0)

# 3. Create a continuous color palette matching your data range
pal <- colorNumeric(
  palette = "YlOrRd", # Yellow-Orange-Red gradient
  domain = all_centers$murder_rate
)

# 4. Render the proportional circle map
leaflet(data = all_centers) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>% 
  setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>% 
  addCircleMarkers(
    lng = ~lon, 
    lat = ~lat,
    # Scales radius proportionally (multiplied by 3 so circles are easily visible)
    radius = ~murder_rate * 3, 
    color = ~pal(murder_rate),    # Border color
    fillColor = ~pal(murder_rate),# Fill color
    fillOpacity = 0.7,
    stroke = TRUE,
    weight = 1,                   # Thin border outline
    # Popup displays both the state and its specific rate on click
    popup = ~paste0("<strong>", state, "</strong><br>Murder Rate: ", round(murder_rate, 2))
  ) %>%
  # Add a legend to the map for context
  addLegend(
    pal = pal, 
    values = ~murder_rate, 
    opacity = 0.7, 
    title = "Murder Rate (per 100k)", 
    position = "bottomright"
  )

