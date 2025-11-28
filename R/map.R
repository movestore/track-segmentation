create_base_map <- function(bbox) {
  leaflet() %>%
    addTiles(group = "OpenStreetMap") %>%
    fitBounds(
      lng1 = bbox[["xmin"]],
      lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]],
      lat2 = bbox[["ymax"]]
    ) |> 
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addDrawToolbar(
      editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
    ) %>% 
    addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers") %>%
    addScaleBar(position = "bottomleft",  # Position of the scale bar...
                options = scaleBarOptions(metric = TRUE,  # Show metric units (meters/kilometers)...
                                          imperial = FALSE,  # Do not show imperial units (feet/miles)...
                                          maxWidth = 400)) |>  # Max width of the scale bar...
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

add_tracking_data <- function(map, data, time_range) {
  # Filter data based on time range....
  filtered_data <- data %>%
    filter(timestamp >= time_range[1] & timestamp <= time_range[2])
  
  # Create unique colors for each animal, NOT USED....
  animals <- unique(filtered_data$animal_id)
  animal_colors <- colorFactor(
    palette = "Set3",
    domain = animals
  )
  
  # Remove existing overlay groups and add new ones....
  map %>%
    clearGroup(group = animals) %>%
    #addRasterImage(
    # geotiff,
    # group="Sentinel",
    #opacity = 0.5
    #)  %>%
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      overlayGroups = animals,
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    {
      m <- .
      for(animal in animals) {
        animal_data <- filtered_data[filtered_data$animal_id == animal,]
        
        # Add track line....
        m <- m %>%
          addPolylines(
            data = animal_data,
            lat = ~latitude,
            lng = ~longitude,
            color = "white",
            weight = 1,
            opacity = 0.3,
            group = animal,
            options = pathOptions(zIndexOffset = 100)
          )
        
        # Add points....
        m <- m %>%
          addCircleMarkers(
            data = animal_data,
            lng = ~longitude,
            lat = ~latitude,
            color = ~pal(locType),
            radius = ~myRadius,
            fillOpacity = 0.8,
            stroke = FALSE,
            popup = ~paste(
              "Animal:", animal_id, "<br>",
              "Species:", species, "<br>",
              "Time:", timestamp, "<br>",
              "Location Type:", locType, "<br>",
              "Location Class:", lc, "<br>",
              "Stop Days:", stop_days, "<br>",
              "N stops:", n_stops, "<br>",
              "Stop ID:", stop_id, "<br>",
              "Coordinates:", round(latitude, 4), ",", round(longitude, 4)
            ),
            group = animal,
            options = pathOptions(zIndexOffset = 200)
          )
      }
      m
    }
}
