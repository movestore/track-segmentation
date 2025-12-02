create_basemap <- function(bbox) {
  leaflet() |>
    addTiles(group = "OpenStreetMap") |>
    fitBounds(
      lng1 = bbox[["xmin"]],
      lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]],
      lat2 = bbox[["ymax"]]
    ) |> 
    addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    addDrawToolbar(
      editOptions = editToolbarOptions(
        selectedPathOptions = selectedPathOptions()
      )
    ) |> 
    addMeasure(
      primaryLengthUnit = "kilometers", 
      secondaryLengthUnit = "kilometers"
    ) |>
    addScaleBar(
      position = "bottomleft",  # Position of the scale bar...
      options = scaleBarOptions(
        metric = TRUE,
        imperial = FALSE, 
        maxWidth = 400
      )
    ) |>
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

add_tracking_lines <- function(map, data) {
  animals <- unique(data$animal_id)
  
  # Add animal IDs as overlay groups
  map <- map |>
    clearGroup(group = animals) |>
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      overlayGroups = animals,
      options = layersControlOptions(collapsed = FALSE)
    )
  
  for (animal in animals) {
    map <- map |>
      addPolylines(
        data = data[data$animal_id == animal, ],
        lat = ~latitude,
        lng = ~longitude,
        color = "white",
        weight = 1,
        opacity = 0.3,
        group = animal,
        options = pathOptions(zIndexOffset = 100)
      )
  }
  
  map
}

add_tracking_points <- function(map, data, init = FALSE) {
  animals <- unique(data$animal_id)
  
  # Add animal IDs as overlay groups
  map <- map |>
    # clearGroup(group = animals) |>
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      overlayGroups = animals,
      options = layersControlOptions(collapsed = FALSE)
    )
  
  if (init) {
    map <- addLocationMarkers(map, data)
  } else {
    map <- addStopMarkers(map, data)
  }
  
  map
}

addLocationMarkers <- function(map, data) {
  for (animal in unique(data$animal_id)) {
    map <- map |> 
      addCircleMarkers(
        data = data[data$animal_id == animal, ],
        lng = ~longitude,
        lat = ~latitude,
        color = "white",
        fillColor = "white",
        radius = 3,
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste(
          "Animal:", animal_id, "<br>",
          "Species:", species, "<br>",
          "Time:", timestamp, "<br>",
          # "Location Type:", locType, "<br>",
          "Location Class:", lc, "<br>",
          # "Stop Days:", stop_days, "<br>",
          # "N stops:", n_stops, "<br>",
          # "Stop ID:", stop_id, "<br>",
          "Coordinates:", round(latitude, 4), ",", round(longitude, 4)
        ),
        group = animal,
        options = pathOptions(zIndexOffset = 200)
      )
  }
  
  map
}

addStopMarkers <- function(map, data) {
  for (animal in unique(data$animal_id)) {
    map <- map |> 
      addCircleMarkers(
        data = data[data$animal_id == animal, ],
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
  
  map
}
