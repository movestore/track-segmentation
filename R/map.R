create_basemap <- function(bbox) {
  leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
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
    ) |> 
    addTrackLegend(
      colors = legend_colors(),
      labels = legend_labels(),
      title = "Location Type"
    )
}

adj_bbox <- function(bbox, dateline = FALSE) {
  bbox_crosses_dateline <- crosses_dateline(bbox)
  
  if (bbox_crosses_dateline && !dateline) {
    # If already crosses, but dateline = FALSE, adjust
    bbox <- c(bbox[3] - 360, bbox[2:1], bbox[4])
  } else if (!bbox_crosses_dateline && dateline) {
    # If doesn't cross, but dateline = TRUE, adjust
    bbox <- c(bbox[3:2], bbox[1] + 360, bbox[4])
  }
  
  bbox
}

crosses_dateline <- function(bbox) {
  bbox[[1]] < 180 & bbox[[3]] > 180
}

addTrackLines <- function(map, data, dateline = FALSE) {
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
        lng = ~get_elon(longitude, dateline = dateline),
        color = "white",
        weight = 1,
        opacity = 0.3,
        group = animal,
        options = pathOptions(zIndexOffset = 100)
      )
  }
  
  map
}

addTrackLocationMarkers <- function(map, data, dateline = FALSE) {
  for (animal in unique(data$animal_id)) {
    map <- map |> 
      addCircleMarkers(
        data = data[data$animal_id == animal, ],
        lng = ~get_elon(longitude, dateline = dateline),
        lat = ~latitude,
        fillColor = unclassified_color(),
        radius = 3,
        fillOpacity = 0.5,
        stroke = FALSE,
        # weight = 2,
        popup = ~paste(
          "Animal:", animal_id, "<br>",
          "Species:", species, "<br>",
          "Time:", timestamp, "<br>",
          "Location Type: Not yet classified <br>",
          "Location Class:", lc, "<br>",
          "Coordinates:", round(latitude, 4), ",", round(longitude, 4)
        ),
        group = animal,
        options = pathOptions(zIndexOffset = 200)
      )
  }
  
  map
}

addTrackStopMarkers <- function(map, data, dateline) {
  pal <- stopover_pal()
  
  for (animal in unique(data$animal_id)) {
    map <- map |> 
      addCircleMarkers(
        data = data[data$animal_id == animal, ],
        lng = ~get_elon(longitude, dateline = dateline),
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

addTrackLayersControl <- function(map, data) {
  animals <- unique(data$animal_id)
  
  # Add animal IDs as overlay groups
  map <- map |>
    # clearGroup(group = animals) |>
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      overlayGroups = animals,
      options = layersControlOptions(collapsed = FALSE)
    )
  
  map
}

addTrackLegend <- function(map, colors, labels, title = "", ...) {
  map |> 
    addControl(
      html = track_legend(colors, labels, title = title),
      position = "bottomright",
      ...
    )
}

track_legend <- function(colors, labels, title = "") {
  stopifnot(length(colors) == length(labels))
  
  html <- paste0(
    "<div class='legend-title'>", title, "</div><div class='custom-legend'>"
  )
  
  for (i in seq_along(colors)) {
    html <- paste0(html, legend_item(colors[i], labels[i]))
  }
  
  html <- paste0(
    html, 
    "<hr>", 
    legend_item(unclassified_color(), "Not yet classified", class = "unclass-pt")
  )
  
  HTML(html)
}

legend_item <- function(color, label, class = NULL) {
  classes <- paste0(c("legend-circle", class), collapse = " ")
  
  paste0(
    "<div class='legend-item'><span class='", classes, "' style='background:", 
    color, ";'></span>", label, "</div>"
  )
}

stopover_pal <- function(colors = lc_colors()) {
  leaflet::colorFactor(colors, unique(stopover_labels()), levels = unique(stopover_labels()))
}

legend_colors <- function() {
  lc_colors()
}

legend_labels <- function() {
  unique(stopover_labels())
}

lc_colors <- function() {
  c("#e31a1c", "#ffde05", "#00f6d0", "#1133f5")
}

unclassified_color <- function() {
  "lightgray"
}
