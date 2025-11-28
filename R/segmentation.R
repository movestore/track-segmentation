
# General data prep to get a move2 object into the format currently expected
# but the segmentation algorithm
move2_to_seg <- function(data) {
  if (sf::st_crs(data) != sf::st_crs("epsg:4326")) {
    data <- sf::st_transform(data, "epsg:4326")
  }
  
  coords <- sf::st_coordinates(data) # likely could adapt code to use geometry col itself
  
  move2::mt_track_id(data) <- "animal_id"
  move2::mt_time(data) <- "timestamp"
  
  data <- data |> 
    dplyr::mutate(
      animal_id = as.character(animal_id), # Factor also works, so this may already be enforced by move2
      latitude = coords[, 2],
      longitude = coords[, 1],
      lc = "G", # Need to deal with this for ARGOS,
      species = "Misc"
    ) |> 
    dplyr::select(animal_id, timestamp, latitude, longitude, lc) |> 
    dplyr::arrange(move2::mt_track_id(data), move2::mt_time(data)) |> 
    na.omit()
  
  data <- data |> 
    dplyr::group_by(animal_id) |> 
    dplyr::filter(dplyr::n() >= 2)
  
  data
}

# Wrapper to find stops for all tracks in a move2 object and recombine output
find_periods_move2 <- function(data, 
                               start_idx, 
                               min_hours,  
                               proximity,
                               max_days = 365000) {
  dl <- data |> 
    dplyr::group_by(animal_id) |>
    dplyr::group_split()
  
  stop_data <- lapply(
    dl,
    function(d) {
      find_periods_recursive(
        d,
        start_idx = start_idx,
        min_hours = min_hours,
        max_days = max_days,
        proximity = proximity
      )
    }
  )
  
  stop_data <- dplyr::bind_rows(stop_data)
  
  stop_data$stop_id <- paste0(
    format(stop_data$start_time, "%Y%m%d%H%M%S"), "_",
    format(stop_data$end_time,   "%Y%m%d%H%M%S"), "_", 
    stop_data$animal_id
  )
  
  data <- dplyr::left_join(
    data,
    stop_data,
    dplyr::join_by(animal_id, timestamp >= start_time, timestamp <= end_time)
  )
  
  duration <- difftime(data$end_time, data$start_time, units = "secs")
  
  # TODO Double check whether these are used in app or just in output.
  data <- data |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      stop_hours = as.numeric(duration / 3600),
      stop_days = as.numeric(duration / (3600 * 24))
    )
  
  data
}

calculate_weighted_loc <- function(data) {
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
  
  data <- data %>%
    mutate(elon_rad = elon * pi / 180) |>
    group_by(stop_id) %>%
    summarize(
      weighted_sin = sum(w * sin(elon_rad)) / sum(w),
      weighted_cos = sum(w * cos(elon_rad)) / sum(w),
      w_circ_mean_rad = atan2(weighted_sin, weighted_cos),
      elon_weighted_mean = w_circ_mean_rad * 180 / pi,
      stopover_lat = weighted.mean(latitude, w),
      n_locs = dplyr::n()
    ) |>
    mutate(stopover_lon = ifelse(elon_weighted_mean > 180,
                                 elon_weighted_mean - 360,
                                 elon_weighted_mean)) |>
    select(stop_id, stopover_lat, stopover_lon, n_locs)
  
  data
}

stops_to_metastops <- function(data) {
  stops <- data |> 
    dplyr::filter(stopover == 1) |> 
    rename(latitude = stopover_lat,
           longitude = stopover_lon)
  
  d1 <- stops |> 
    mutate(timestamp = start_time)
  
  d2 <- stops |> 
    mutate(timestamp = end_time)
  
  rbind(d1, d2) |> 
    dplyr::select(animal_id,
                  timestamp,
                  latitude,
                  longitude,
                  species,
                  n_locs) |> 
    na.omit() |> 
    arrange(animal_id, timestamp) %>%
    group_by(animal_id) %>%
    filter(n() >= 2)
}

# Has to do lots of same things as calculate_weighted_loc unfortunately
# could add an arg to indicate different selection strategy there
# (e.g. select largest n stopover rather than weighting GPS locs)
get_metastop_loc <- function(data) {
  stops <- data |> 
    filter(!is.na(start_time)) |> 
    group_by(metastop_id) |> 
    mutate(n_meta = dplyr::n() / 2) |> 
    filter(n_locs == max(n_locs)) |> # TODO confirm this is fastre than arrange() and filter by row number
    filter(timestamp == min(timestamp)) |> 
    mutate(gis_lat = latitude, gis_lon = longitude) %>%
    mutate(meta_lat = latitude, meta_lon = longitude) %>%
    dplyr::select(metastop_id,meta_lat,meta_lon,n_meta,gis_lat,gis_lon)
  
  stops <- dplyr::left_join(
    data,
    stops,
    dplyr::join_by(metastop_id)
  )
  
  stops |> 
    dplyr::arrange(metastop_id, timestamp) |> 
    group_by(metastop_id) |> 
    filter(row_number() == 1) |> 
    mutate(metastop = 10) |> 
    rename(metastart_time = start_time, metaend_time = end_time) |> 
    dplyr::select(animal_id,metastop,n_meta,metastart_time,metaend_time,
                  metastop_id,metastop_hours,metastop_days,
                  meta_lat,meta_lon)
}

tidy_metastop_data <- function(data, dateline = FALSE) {
  metastops <- data |> 
    filter(metastop == 10) |> 
    arrange(animal_id, metastart_time) |>  # May not be necessary
    group_by(metastop_id) |> 
    filter(row_number() == 1) |> 
    ungroup() |> 
    mutate(stopover = 3, stop_id = metastop_id, gis_lat = meta_lat, gis_lon = meta_lon,
           stop_hours = metastop_hours, stop_days = metastop_days,
           n_locs = n_meta, timestamp = metastart_time,
           start_time = metastart_time, end_time = metaend_time) |>
    dplyr::select(-n_meta,-metastart_time,-metaend_time,-metastop_id,-metastop_hours,-metastop_days,
                  -meta_lat,-meta_lon)
  
  non_metastops <- data |>
    dplyr::select(
      -n_meta,
      -metastart_time,
      -metaend_time,
      -metastop_id,
      -metastop_hours,
      -metastop_days,
      -meta_lat,
      -meta_lon
    )
  
  rbind(non_metastops, metastops) |> 
    arrange(animal_id, timestamp) |> 
    mutate(
      stopover = metastop + stopover,
      gis_elon = ifelse(gis_lon < 0 & dateline, gis_lon + 360, gis_lon)
    ) |> 
    filter(stopover != 11) |> 
    mutate(
      original_lat = ifelse(stopover == 13, NA, original_lat),
      original_lon = ifelse(stopover == 13, NA, original_lon),
      lc = ifelse(stopover == 13, NA, lc)
    ) |> 
    select(
      animal_id,timestamp,start_time,end_time,
      stop_id,stop_days,stopover,gis_lat,gis_lon,gis_elon,
      n_locs,lc,species, -stop_hours, -metastop
    ) |> 
    arrange(animal_id, timestamp, desc(stopover))
}

data_for_leaflet <- function(data) {
  # Validate required columns exist in the dataset...
  required_cols <- c("animal_id", "species", "stopover", "gis_lat", "gis_elon", "n_locs", "timestamp")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop("Error: Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # message("Number of locations that will be processed: ", nrow(data))
  
  data <- data |> 
    mutate(
      animal_id = as.factor(animal_id),
      myRadius = stopover_to_radius(stopover),
      locType = stopover_to_label(stopover),
      latitude = gis_lat,
      longitude = gis_elon,
      n_stops = n_locs,
      myRadius = if_else(stopover == 13, ((n_stops / 10 * myRadius) + myRadius), myRadius)
    ) |> 
    dplyr::select(animal_id, timestamp, latitude, longitude, lc, stop_days, locType, myRadius, stop_id, n_stops, species) %>%
    ungroup()
  
  # Validate and filter coordinates...
  data <- data |> 
    filter(latitude >= -90 & latitude <= 90) |> 
    filter(longitude >= -180 & longitude <= 360)
  
  # Sort by animal_id and timestamp...
  data <- data |> 
    arrange(animal_id, timestamp)
  
  data
}

stopover_pal <- function(colors = c("red", "blue", "cyan", "yellow")) {
  leaflet::colorFactor(colors, unique(stopover_labels()))
}

lc_colors <- function(colors = c("red", "blue", "cyan", "yellow")) {
  # leaflet maps colors by sorted position, not index
  setNames(colors, sort(unique(stopover_labels())))
}

lc_recode <- function(x) {
  key_recode(x, lc_weights(), 1)
}

stopover_to_radius <- function(x) {
  key_recode(x, stopover_radii(), 99)
}

stopover_to_label <- function(x) {
  key_recode(x, stopover_labels(), "Unclassified")
}

lc_weights <- function() {
  c(
    "G" = 5000,
    "4" = 5000,
    "3" = 1000,
    "2" = 500,
    "1" = 300,
    "0" = 50,
    "A" = 100,
    "B" = 10,
    "Z" = 5
  )
}

stopover_radii <- function() {
  c(
    "13" = 10,
    "12" = 3,
    "10" = 3,
    "2"  = 3,
    "0"  = 3
  )
}

stopover_labels <- function() {
  c(
    "13" = "MetaStop",
    "12" = "Stopped",
    "10" = "MovementDuringStop",
    "2"  = "Movement",
    "0"  = "Movement"
  )
}

key_recode <- function(x, key, replace = NA) {
  y <- unname(key[as.character(x)])
  y[is.na(y)] <- replace
  y
}
