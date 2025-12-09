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

# Adjust bbox to cross/not cross dateline depending on current crossing status
adj_bbox <- function(bbox, should_cross_dl) {
  crosses_dl <- bbox$crosses_dl
  bbox_adj <- bbox$bbox
  
  if (crosses_dl && !should_cross_dl) {
    # If already crosses, but dateline = FALSE, adjust
    bbox_adj <- c(bbox_adj[3] - 360, bbox_adj[2:1], bbox_adj[4])
    crosses_dl <- FALSE
  } else if (!crosses_dl && should_cross_dl) {
    # If doesn't cross, but dateline = TRUE, adjust
    bbox_adj <- c(bbox_adj[3:2], bbox_adj[1] + 360, bbox_adj[4])
    crosses_dl <- TRUE
  }
  
  list(
    bbox = bbox_adj,
    crosses_dl = crosses_dl
  )
}

# Intersect a set of LINESTRINGS with the international dateline to determine
# if tracks automatically cross or not
st_crosses_dateline <- function(data) {
  any(sf::st_intersects(data, sfc_idl(), sparse = FALSE))
}

sfc_idl <- function() {
  idl_coords <- matrix(c(180, -90, 180, 90), nrow = 2, byrow = TRUE)
  
  sf::st_sfc(
    sf::st_linestring(idl_coords),
    crs = 4326
  )
}

# Initial bbox for input data. Determine whether the data cross the dateline
# for correct map initialization.
get_init_bbox <- function(data) {
  bbox <- list(
    bbox = sf::st_bbox(data),
    crosses_dl = FALSE
  )
  
  crosses_dl <- data |> 
    dplyr::group_by(move2::mt_track_id(data)) |> 
    dplyr::summarize(
      n = n(),
      geometry = sf::st_combine(geometry)
    ) |> 
    dplyr::filter(n > 1) |> # Can't build linestring from single point
    sf::st_cast("LINESTRING") |> 
    st_crosses_dateline()
  
  adj_bbox(bbox, should_cross_dl = crosses_dl)
}

addTrackLines <- function(map, data) {
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
        lng = ~longitude_adj,
        color = "white",
        weight = 1,
        opacity = 0.3,
        group = animal,
        options = pathOptions(zIndexOffset = 100)
      )
  }
  
  map
}

addTrackLocationMarkers <- function(map, data) {
  for (animal in unique(data$animal_id)) {
    map <- map |> 
      addCircleMarkers(
        data = data[data$animal_id == animal, ],
        lng = ~longitude_adj,
        lat = ~latitude,
        fillColor = unclassified_color(),
        radius = 3,
        fillOpacity = 0.5,
        stroke = FALSE,
        # weight = 2,
        popup = ~paste(
          bold("Animal:"), animal_id, "<br>",
          bold("Species:"), species, "<br>",
          bold("Time:"), timestamp, "<br>",
          bold("Location Type:"), "Not yet classified <br>",
          bold("Location Class:"), lc, "<br>",
          bold("Coordinates:"), round(latitude, 4), ",", round(longitude, 4)
        ),
        group = animal,
        options = pathOptions(zIndexOffset = 200)
      )
  }
  
  map
}

addTrackStopMarkers <- function(map, data) {
  pal <- stopover_pal()
  
  for (animal in unique(data$animal_id)) {
    map <- map |> 
      addCircleMarkers(
        data = data[data$animal_id == animal, ],
        lng = ~longitude_adj,
        lat = ~latitude,
        color = ~pal(locType),
        radius = ~myRadius,
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = ~paste(
          bold("Animal:"), animal_id, "<br>",
          bold("Species:"), species, "<br>",
          bold("Time:"), timestamp, "<br>",
          bold("Location Type:"), locType, "<br>",
          bold("Location Class:"), lc, "<br>",
          bold("Stop Days:"), stop_days, "<br>",
          bold("N stops:"), n_stops, "<br>",
          bold("Stop ID:"), stop_id, "<br>",
          bold("Coordinates:"), round(latitude, 4), ",", round(longitude, 4)
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

bold <- function(x) {
  paste0("<strong>", x, "</strong>")
}
