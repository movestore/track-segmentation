# Build basemap centered on track bbox with relevant overlay groups and other
# widgets
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
      position = "bottomleft", # Position of the scale bar...
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
    ) |>
    addMapPane("metastops", zIndex = 400) |>
    addMapPane("movement", zIndex = 420) |>
    addMapPane("stops", zIndex = 450)
}

# Intersect a set of LINESTRINGS with the international dateline to determine
# if tracks cross the dateline or not
st_crosses_dateline <- function(data) {
  any(sf::st_intersects(data, sfc_idl(), sparse = FALSE))
}

# International dateline sfc object (fixed at 180 degree meridian)
sfc_idl <- function() {
  idl_coords <- matrix(c(180, -90, 180, 90), nrow = 2, byrow = TRUE)

  sf::st_sfc(
    sf::st_linestring(idl_coords),
    crs = 4326
  )
}

# Determine whether any tracks in an input move2 object cross the
# international dateline. Groups by track id, casts track points to LINESTRING,
# and intersects with dateline
move2_crosses_dateline <- function(data) {
  data |>
    dplyr::group_by(move2::mt_track_id(data)) |>
    dplyr::summarize(
      n = n(),
      geometry = sf::st_combine(geometry)
    ) |>
    dplyr::filter(n > 1) |> # Can't build linestring from single point
    sf::st_cast("LINESTRING") |>
    st_crosses_dateline()
}

# Get an initial bounding box for the input segmentation data based on
# whether tracks cross the dateline or not
get_init_bbox <- function(data, dateline) {
  elon_range <- range(get_elon(data$longitude, dateline))
  lat_range <- range(data$latitude)

  bbox <- sf::st_bbox(
    c(
      xmin = min(elon_range),
      ymin = min(lat_range),
      xmax = max(elon_range),
      ymax = max(lat_range)
    )
  )

  bbox
}

# Wrapper to add linestrings connecting track points as a map layer
addTrackLines <- function(map, data) {
  animals <- unique(data$animal_id)

  # Add animal IDs as overlay groups
  map <- map |>
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

# Wrapper to add circle markers with indicated aesthetics for initial
# unclassified input data. That is, this plots raw location points
# on the map. Used in map initialization to ensure that track locations are
# visible before stops have been identified.
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
        popup = ~ paste(
          bold("Animal:"), animal_id, "<br>",
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

# Wrapper to add segmentation result markers with specified z-index layering
addTrackStopMarkers <- function(map, data) {
  for (animal in unique(data$animal_id)) {
    data_animal <- data[data$animal_id == animal, ]

    data_meta <- filter(data_animal, locType == "Metastop")
    data_stop <- filter(data_animal, locType == "Stopped")
    data_mvmt <- filter(data_animal, !locType %in% c("Metastop", "Stopped"))

    map <- map |>
      addTrackStopMarkersLayer(data_meta, group = animal, pane = "metastops") |>
      addTrackStopMarkersLayer(data_stop, group = animal, pane = "stops") |>
      addTrackStopMarkersLayer(data_mvmt, group = animal, pane = "movement")
  }

  map
}

# Wrapper to add segmentation result markers with specified aesthetics
addTrackStopMarkersLayer <- function(map,
                                     data,
                                     group,
                                     pane,
                                     pal = stopover_pal()) {
  # Check nrow to handle possibility that certain stop types were not identified
  # with the given input parameters
  if (nrow(data) > 0) {
    map <- addCircleMarkers(
      map,
      data = data,
      lng = ~longitude_adj,
      lat = ~latitude,
      color = ~ pal(locType),
      radius = ~myRadius,
      fillOpacity = 0.8,
      stroke = FALSE,
      popup = ~ paste(
        bold("Animal:"), animal_id, "<br>",
        bold("Time:"), timestamp, "<br>",
        bold("Location Type:"), locType, "<br>",
        bold("Location Class:"), lc, "<br>",
        bold("Stop Days:"), stop_days, "<br>",
        bold("N stops:"), n_stops, "<br>",
        bold("Stop ID:"), stop_id, "<br>",
        bold("Coordinates:"), round(latitude, 4), ",", round(longitude, 4)
      ),
      group = group,
      options = pathOptions(pane = pane)
    )
  }

  map
}

# Add animal IDs as overlay groups
addTrackLayersControl <- function(map, data) {
  animals <- unique(data$animal_id)

  map <- map |>
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      overlayGroups = animals,
      options = layersControlOptions(collapsed = FALSE)
    )

  map
}

# Add custom legend based on the given color palette
addTrackLegend <- function(map, colors, labels, title = "", ...) {
  map |>
    addControl(
      html = track_legend(colors, labels, title = title),
      position = "bottomright",
      ...
    )
}

# Build custom HTML legend for the given location type classes
# Use custom legend to add an additional class for "unclassified" points
# which will be present on map initialization (before segmentation has run).
# Also adds classes to facilitate custom CSS styling.
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

# Build a single legend item for a given color and label
legend_item <- function(color, label, class = NULL) {
  classes <- paste0(c("legend-circle", class), collapse = " ")

  paste0(
    "<div class='legend-item'><span class='", classes, "' style='background:",
    color, ";'></span>", label, "</div>"
  )
}

# Helpers to build palette for segmented location classes
stopover_pal <- function(colors = legend_colors()) {
  leaflet::colorFactor(colors, unique(stopover_labels()), levels = unique(stopover_labels()))
}

legend_colors <- function() {
  c("#e31a1c", "#ffde05", "#00f6d0", "#1133f5")
}

legend_labels <- function() {
  unique(stopover_labels())
}

unclassified_color <- function() {
  "lightgray"
}

bold <- function(x) {
  paste0("<strong>", x, "</strong>")
}
