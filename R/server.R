
app_version <- function() {
  "014"
}

check_data_frame <- function(df) {
  required_columns <- c("animal_id", "timestamp", "latitude", "longitude", "lc", "species")
  
  # Check for presence of all required columns
  if (!all(required_columns %in% names(df))) {
    missing_cols <- setdiff(required_columns, names(df))
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Check types
  if (!is.character(df$animal_id)) stop("animal_id must be of type character.")
  if (!inherits(df$timestamp, "POSIXct")) stop("timestamp must be of class POSIXct.")
  if (!is.numeric(df$latitude)) stop("latitude must be numeric.")
  if (!is.numeric(df$longitude)) stop("longitude must be numeric.")
  if (!is.character(df$lc)) stop("lc must be of type character.")
  if (!is.character(df$species)) stop("species must be of type character.")
  
  # Check value ranges
  if (any(df$latitude < -90 | df$latitude > 90, na.rm = TRUE)) {
    stop("latitude values must be between -90 and 90.")
  }
  if (any(df$longitude < -180 | df$longitude > 180, na.rm = TRUE)) {
    stop("longitude values must be between -180 and 180.")
  }
  
  # Check 'lc' values
  valid_lc <- c("3", "2", "1", "0", "A", "B", "Z", "G")
  if (!all(df$lc %in% valid_lc)) {
    invalid_lc <- unique(df$lc[!df$lc %in% valid_lc])
    stop(paste("Invalid lc values found:", paste(invalid_lc, collapse = ", ")))
  }
  
  message("\nData frame passed all checks.")
  return(TRUE)
}

sanitize_studyName <- function(studyName) {
  # Invalid Windows filename characters...
  invalid_chars <- "[<>:\"/\\\\|?*]"
  # Replace invalid characters with an underscore...
  cleaned_studyName <- gsub(invalid_chars, "_", studyName)
  return(cleaned_studyName)
}

find_periods_recursive <- function(df, start_idx, min_hours, max_days, proximity) {
  if(length(start_idx) == 0) start_idx <- nrow(df)
  if (start_idx >= nrow(df)) {
    return(NULL)
  }
  
  results <- list()
  current_start <- start_idx
  
  while(current_start < nrow(df)) {
    # Initialize variables for current period....
    current_idx <- current_start
    valid_period <- FALSE
    
    # Iterative replacement for check_next_point....
    while(current_idx < nrow(df)) {
      # Pre-calculate coordinates matrices for efficiency....
      prev_coords <- matrix(c(df$longitude[current_start:current_idx],
                              df$latitude[current_start:current_idx]), ncol = 2)
      next_coords <- matrix(c(df$longitude[current_idx + 1],
                              df$latitude[current_idx + 1]), ncol = 2)
      
      # Calculate distances....
      distances <- geosphere::distHaversine(prev_coords, next_coords)
      
      # Check if all distances are within threshold....
      if (all(distances < proximity)) {
        time_diff <- as.numeric(difftime(df$timestamp[current_idx + 1],
                                         df$timestamp[current_start],
                                         units = "hours"))
        
        if (time_diff <= (max_days * 24)) {
          current_idx <- current_idx + 1
          next
        }
      }
      break
    }
    
    # Check if period is valid....
    if (current_idx > current_start) {
      time_diff <- as.numeric(difftime(df$timestamp[current_idx],
                                       df$timestamp[current_start],
                                       units = "hours"))
      
      if (time_diff >= min_hours) {
        results[[length(results) + 1]] <- data.frame(
          animal_id = df$animal_id[current_start],
          start_time = df$timestamp[current_start],
          end_time = df$timestamp[current_idx],
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Move to next potential period....
    current_start <- current_idx + 1
    
    # Add safety check to prevent infinite loops....
    if (current_start >= nrow(df) || current_idx >= nrow(df)) {
      break
    }
  }
  
  # Combine all results....
  if (length(results) > 0) {
    return(do.call(rbind, results))
  } else {
    return(NULL)
  }
}

calculate_weighted_longitude <- function(data) {
  # Detect if longitudes cross 180 or 0....
  lon_range <- range(data$longitude, na.rm = TRUE)
  
  if ((lon_range[2] - lon_range[1]) > 180) {
    # If longitude range is > 180, assume crossing the 180 dateline...
    data <- data %>%
      mutate(elon = ifelse(longitude < 0, longitude + 360, longitude))
  } else {
    # Otherwise, use the standard [-180, 180] range for Greenwich crossing
    data <- data %>%
      mutate(elon = ifelse(longitude > 180, longitude - 360, longitude))
  }
  
  # Compute weighted circular mean
  data <- data %>%
    group_by(stop_id) %>%
    mutate(elon_rad = elon * pi / 180,
           weighted_sin = sum(w * sin(elon_rad)) / sum(w),
           weighted_cos = sum(w * cos(elon_rad)) / sum(w),
           w_circ_mean_rad = atan2(weighted_sin, weighted_cos),
           elon_weighted_mean = w_circ_mean_rad * 180 / pi)
  
  # Select representative stopover per stop_id
  data <- data %>%
    arrange(stop_id, timestamp) %>%
    filter(row_number() == 1) %>%
    mutate(stopover_lon = ifelse(elon_weighted_mean > 180, 
                                 elon_weighted_mean - 360, 
                                 elon_weighted_mean)) %>%
    select(stop_id, stopover_lon)
  
  return(data)
}


prepare_data <- function() {
  # Check if required objects exist...
  if (!exists("ready4")) stop("Error: 'ready4' dataset is missing.")
  if (!exists("subsetAnimals")) subsetAnimals <- FALSE
  if (!exists("listAnimals")) listAnimals <- NULL
  
  METAstopovers_v10 <- ready4
  message("Number of location records before subsetting: ", nrow(ready4))
  
  # Subsetting based on animal IDs if subsetAnimals is TRUE...
  if (subsetAnimals) {
    if (is.null(listAnimals) || length(listAnimals) == 0) {
      stop("Error: 'listAnimals' is missing or empty while 'subsetAnimals' is TRUE.")
    }
    METAstopovers_v10 <- ready4 %>%
      filter(animal_id %in% listAnimals)
    
    message("=======================================================================")
    message("Subsetting to include these ", length(listAnimals), " animal_ids only:")
    print(listAnimals)
    message("=======================================================================")
  } else {
    message("=======================================================================")
    message("No subsetting requested, so including all animal_ids....")
    message("=======================================================================")
  }
  
  message("Number of locations that will be processed: ", nrow(METAstopovers_v10))
  
  # Validate required columns exist in the dataset...
  required_cols <- c("animal_id", "species", "stopover", "gis_lat", "gis_elon", "n_locs", "timestamp")
  missing_cols <- setdiff(required_cols, colnames(METAstopovers_v10))
  if (length(missing_cols) > 0) {
    stop("Error: Missing required columns in 'METAstopovers_v10': ", paste(missing_cols, collapse = ", "))
  }
  
  # Prepare the Leaflet data...
  dataLeaflet <- METAstopovers_v10 %>%
    mutate(
      myRadius =if_else(stopover == 13, 10,
                        if_else(stopover == 12, 3,
                                if_else(stopover == 10, 3,
                                        if_else(stopover == 2, 3,
                                                if_else(stopover == 0, 3, 99))))),
      locType = if_else(stopover == 13, "MetaStop",
                        if_else(stopover == 12, "Stopped",
                                if_else(stopover == 10, "MovementDuringStop",
                                        if_else(stopover ==  2, "Movement",
                                                if_else(stopover ==  0, "Movement", "Unclassifed"))))),
      latitude = gis_lat,
      longitude = gis_elon,
      n_stops = n_locs,
      myRadius = if_else(stopover == 13, ((n_stops / 10 * myRadius) + myRadius), myRadius)
    ) %>%
    dplyr::select(animal_id, timestamp, latitude, longitude, lc, stop_days, locType, myRadius, stop_id, n_stops, species) %>%
    ungroup()
  
  dataLeaflet$animal_id <- as.factor(dataLeaflet$animal_id)
  
  # Validate and filter coordinates...
  dataLeaflet <- dataLeaflet %>%
    filter(latitude >= -90 & latitude <= 90) %>%
    filter(longitude >= -180 & longitude <= 360)
  
  # Sort by animal_id and timestamp...
  dataLeaflet <- dataLeaflet %>%
    arrange(animal_id, timestamp)
  
  message("Data preparation complete. Ready for visualization.")
  
  # Define color palettes....
  pal <- colorFactor(c("red", "blue", "cyan", "yellow"),
                     domain = c("MetaStop", "Stopped", "MovementDuringStop", "Movement"))
  
  lc_colors <- c(
    "MetaStop" = "red",
    "Stopped" = "yellow",
    "MovementDuringStop" = "cyan",
    "Movement" = "blue"
  )
  
  ts_min <- as.POSIXct(min(dataLeaflet$timestamp))
  ts_max <- as.POSIXct(min(dataLeaflet$timestamp))
  
  # Return the prepared data and palettes...
  return(list(dataLeaflet = dataLeaflet, palette = pal, lc_colors = lc_colors,
              ts_min = ts_min, ts_max = ts_max))
  
}


create_base_map <- function() {
  leaflet(options = leafletOptions(maxZoom = 22)) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addLayersControl(
      baseGroups = c( "Satellite", "OpenStreetMap"),
      overlayGroups = character(0),  # Start with empty overlay groups...
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addDrawToolbar(
      editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
    ) %>% 
    addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers") %>%
    addScaleBar(position = "bottomleft",  # Position of the scale bar...
                options = scaleBarOptions(metric = TRUE,  # Show metric units (meters/kilometers)...
                                          imperial = FALSE,  # Do not show imperial units (feet/miles)...
                                          maxWidth = 400))  # Max width of the scale bar...
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


summarize_tracking_data <- function(dataWork) {
  # Ensure necessary columns exist
  required_cols <- c("animal_id", "timestamp", "gis_lat", "gis_lon")
  missing_cols <- setdiff(required_cols, colnames(dataWork))
  if (length(missing_cols) > 0) {
    stop("Error: Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  nrow(dataWork)
  # Sort dataWork by animal_id and timestamp
  dataWork <- dataWork %>%
    arrange(animal_id, timestamp) %>%
    group_by(animal_id) %>%
    filter(n() >= 2) %>%
    select(animal_id, timestamp, gis_lat, gis_lon) %>%
    na.omit() %>%
    ungroup()
  nrow(dataWork)
  
  # Function to calculate total distance moved by an animal
  calculate_distance <- function(latitudes, longitudes) {
    # Using geosphere::distVincentySphere to calculate distances
    dist <- distVincentySphere(cbind(longitudes[-length(longitudes)], latitudes[-length(latitudes)]),
                               cbind(longitudes[-1], latitudes[-1]))
    sum(dist, na.rm = TRUE)  # Sum of all distances
  }
  
  # Summarize dataWork by animal_id....
  summary <- dataWork %>%
    group_by(animal_id) %>%
    summarise(
      start_date = min(timestamp),
      end_date = max(timestamp),
      tot_days = round(as.numeric(difftime(max(timestamp), min(timestamp), units = "days")),0),
      tot_dist_km = (calculate_distance(gis_lat, gis_lon)) / 1000.,
      n_locs = round(n(),0),
      percent = round((n_locs / nrow(dataWork)) * 100, 2)
    ) %>%
    arrange(desc(n_locs)) %>%
    ungroup()
  
  return(summary)
}

