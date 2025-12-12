# General data prep to get a move2 object into the format currently expected
# by the segmentation algorithm. Algorithm expects a data frame with
# `animal_id`, `timestamp`, `latitude`, `longitude` and `lc` columns, ordered
# within `animal_id` and `timestamp`, in WGS84.
move2_to_seg <- function(data) {
  if (st_crs(data) != st_crs("epsg:4326")) {
    data <- st_transform(data, "epsg:4326")
  }

  coords <- st_coordinates(data)

  # Set hard-coded colnames for track id and timestamp column. These will be
  # used to refer to these columns throughout the app.
  move2::mt_track_id(data) <- "animal_id"
  move2::mt_time(data) <- "timestamp"

  # If the data contains ARGOS lc records, use those. If not, use NA for ARGOS
  # records. "G" is always used for non-ARGOS records.
  if (has_argos_lc(data)) {
    data <- data |>
      mutate(lc = ifelse(sensor_type_id == 82798, as.character(argos_lc), "G"))
  } else {
    data <- data |>
      mutate(lc = ifelse(sensor_type_id == 82798, NA, "G"))
  }

  data <- data |>
    st_drop_geometry() |>
    as_tibble() |> # drop move2 class
    mutate(
      animal_id = as.character(animal_id),
      latitude = coords[, 2],
      longitude = coords[, 1]
    ) |>
    select(animal_id, timestamp, latitude, longitude, lc) |>
    arrange(animal_id, timestamp) |>
    na.omit()

  data
}

check_seg_data <- function(df) {
  required_columns <- c("animal_id", "timestamp", "latitude", "longitude", "lc")

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

  # Check value ranges
  if (any(df$latitude < -90 | df$latitude > 90, na.rm = TRUE)) {
    stop("latitude values must be between -90 and 90.")
  }
  if (any(df$longitude < -180 | df$longitude > 180, na.rm = TRUE)) {
    stop("longitude values must be between -180 and 180.")
  }

  # Check 'lc' values
  valid_lc <- names(lc_weights())

  if (!all(df$lc %in% valid_lc)) {
    invalid_lc <- unique(df$lc[!df$lc %in% valid_lc])
    stop(paste("Invalid lc values found:", paste(invalid_lc, collapse = ", ")))
  }

  df
}

# Run the segmentation algorithm with a set proximity and duration to identify
# stop locations. This wraps `identify_stops()` (which actually runs the
# stop location identification process) and annotates raw locations with
# their stop information, if any. It also calculates the weighted location of
# each stop. Input data should be the raw seg data as produced by
# `move2_to_seg()`
find_stop_locations <- function(data, min_hours, proximity) {
  data2 <- identify_stops(
    data,
    start_idx = 1,
    min_hours = min_hours,
    proximity = proximity
  )
  
  # isolote and annotate raw locations that were not stopped
  transit_locs <- data2 |>
    filter(is.na(stop_id)) |>
    mutate(
      stopover = 0,
      original_lat = latitude,
      original_lon = longitude,
      gis_lat = latitude,
      gis_lon = longitude
    ) |>
    select(-latitude, -longitude)
  
  # isolate and annotate the raw locations that were stopped
  stopped_locs <- data2 |>
    filter(!is.na(stop_id)) |>
    mutate(
      stopover = 2,
      original_lat = latitude,
      original_lon = longitude,
      gis_lat = original_lat,
      gis_lon = original_lon,
      w = lc_recode(lc)
    ) |>
    select(-latitude, -longitude)
  
  # Calculate weighted stop locations
  weighted_stopped_locs <- stopped_locs |>
    mutate(
      stopover_lat = weighted_lat(original_lat, w),
      stopover_lon = weighted_lon(original_lon, w),
      n_locs = n(),
      .by = stop_id
    ) |>
    filter(timestamp == min(timestamp), .by = stop_id) |>
    mutate(
      stopover = 1,
      lc = NA,
      original_lat = NA,
      original_lon = NA,
      gis_lat = NA,
      gis_lon = NA
    )
  
  # Recombine raw transit locations with annotated stop locations and
  # weighted stop locations
  stops_data <- bind_rows(
    transit_locs,
    stopped_locs,
    weighted_stopped_locs
  ) |>
    select(-w) |>
    arrange(animal_id, timestamp)
  
  stops_data
}

# Run the segmentation algorithm with a set proximity and duration to identify
# metastop locations. This prepares the output of the stop identification
# step to be passed back through the segmentation algorithm (`identify_stops()`)
# Because the results of the stop identification include annotated observed
# location data, but we want to re-run the algorithm only on the weighted
# stop locations themselves, we must massage the data before passing back through
# the algorithm. Input data should be the results of `find_stop_locations()`
find_metastop_locations <- function(data, min_hours, proximity) {
  # Select weighted stop locations and reformat data
  meta <- stops_to_metastops(data)
  
  # Re-run the algorithm on the weighted stop locations
  meta <- identify_stops(
    meta,
    start_idx = 1,
    min_hours = min_hours,
    proximity = proximity
  ) |>
    rename(
      metastop_id = stop_id,
      metastop_hours = stop_hours,
      metastop_days = stop_days
    )
  
  # Attach relevant metadata to each metastop (number of associated stop
  # locations, first stop timestamp) and reformat
  meta <- get_metastop_stats(meta)
  
  meta <- left_join(
    data,
    meta,
    join_by(animal_id, timestamp >= metastart_time, timestamp <= metaend_time)
  ) |>
    mutate(metastop = replace_na(metastop, 0))
  
  tidy_metastop_data(meta)
}

# Segmentation algorithm implementation for single track
identify_stops_ <- function(df, start_idx, min_hours, max_days, proximity) {
  if (length(start_idx) == 0) {
    start_idx <- nrow(df)
  }

  if (start_idx >= nrow(df)) {
    return(NULL)
  }

  results <- list()
  current_start <- start_idx

  while (current_start < nrow(df)) {
    # Initialize variables for current period
    current_idx <- current_start
    valid_period <- FALSE

    # Iterative replacement for check_next_point
    while (current_idx < nrow(df)) {
      # Pre-calculate coordinates matrices for efficiency
      prev_coords <- matrix(
        c(
          df$longitude[current_start:current_idx],
          df$latitude[current_start:current_idx]
        ),
        ncol = 2
      )

      next_coords <- matrix(
        c(
          df$longitude[current_idx + 1],
          df$latitude[current_idx + 1]
        ),
        ncol = 2
      )

      # Calculate distances
      distances <- geosphere::distHaversine(prev_coords, next_coords)

      # Check if all distances are within threshold
      if (all(distances < proximity)) {
        time_diff <- as.numeric(difftime(df$timestamp[current_idx + 1],
          df$timestamp[current_start],
          units = "hours"
        ))

        if (time_diff <= (max_days * 24)) {
          current_idx <- current_idx + 1
          next
        }
      }
      break
    }

    # Check if period is valid
    if (current_idx > current_start) {
      time_diff <- as.numeric(difftime(df$timestamp[current_idx],
        df$timestamp[current_start],
        units = "hours"
      ))

      if (time_diff >= min_hours) {
        cur_animal_id <- df$animal_id[current_start]
        start_ts <- df$timestamp[current_start]
        end_ts <- df$timestamp[current_idx]
        stop_id <- paste0(
          format(start_ts, "%Y%m%d%H%M%S"), "_",
          format(end_ts, "%Y%m%d%H%M%S"), "_",
          cur_animal_id
        )

        results[[length(results) + 1]] <- data.frame(
          animal_id = cur_animal_id,
          start_time = start_ts,
          end_time = end_ts,
          stop_id = stop_id,
          stringsAsFactors = FALSE
        )
      }
    }

    # Move to next potential period
    current_start <- current_idx + 1

    # Add safety check to prevent infinite loops
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

# Wrapper to find stops for all tracks in a move2 object and recombine output
identify_stops <- function(data,
                           start_idx,
                           min_hours,
                           proximity,
                           max_days = 365000) {
  dl <- data |>
    group_by(animal_id) |>
    group_split()

  stops <- bind_rows(
    lapply(
      dl,
      function(d) {
        identify_stops_(
          d,
          start_idx = start_idx,
          min_hours = min_hours,
          max_days = max_days,
          proximity = proximity
        )
      }
    )
  )

  duration <- difftime(stops$end_time, stops$start_time, units = "secs")

  stops <- stops |>
    mutate(
      stop_hours = as.numeric(duration / 3600),
      stop_days = as.numeric(duration / (3600 * 24))
    )

  stop_data <- left_join(
    data,
    stops,
    join_by(animal_id, timestamp >= start_time, timestamp <= end_time)
  )

  stop_data
}

# Calculate weighted longitude
weighted_lon <- function(lon, weight, dateline = FALSE) {
  # CHECK: is this sufficient replacement for previous dateline heuristic used here?
  elon <- get_elon(lon, dateline = dateline)

  elon_rad <- elon * pi / 180

  weighted_sin <- sum(weight * sin(elon_rad)) / sum(weight)
  weighted_cos <- sum(weight * cos(elon_rad)) / sum(weight)

  w_circ_mean_rad <- atan2(weighted_sin, weighted_cos)
  elon_weighted_mean <- w_circ_mean_rad * 180 / pi

  stopover_lon <- ifelse(
    elon_weighted_mean > 180,
    elon_weighted_mean - 360,
    elon_weighted_mean
  )

  stopover_lon
}

# Calculate weighted latitude
weighted_lat <- function(lat, weight) {
  weighted.mean(lat, weight)
}

# Prepare output of initial stops segmentation for metastops identification
# To identify metastops, we want to run only stop locations back through
# the segmentation algorithm, so we remove non-stop locations, reset
# the lat/lon locations to the weighted stop locations, and expand the
# data long-wise to include individual stop records for the start and end
# of each stop, as these will be used to determine metastop duration.
stops_to_metastops <- function(data) {
  stops <- data |>
    filter(stopover == 1) |> # Select only stopovers
    rename(latitude = stopover_lat, longitude = stopover_lon)

  d1 <- stops |>
    mutate(timestamp = start_time)

  d2 <- stops |>
    mutate(timestamp = end_time)

  rbind(d1, d2) |>
    select(
      animal_id,
      timestamp,
      latitude,
      longitude,
      n_locs
    ) |>
    na.omit() |>
    arrange(animal_id, timestamp) |>
    group_by(animal_id) |>
    filter(n() >= 2)
}

# Wrapper to augment output of metastop segmentation results with number of
# stop locations and reorganize data frame for output
get_metastop_stats <- function(data) {
  stops <- data |>
    filter(!is.na(metastop_id)) |>
    group_by(metastop_id) |>
    mutate(n_meta = n() / 2) |>
    filter(n_locs == max(n_locs)) |> # TODO confirm this is faster than arrange() and filter by row number
    filter(timestamp == min(timestamp)) |>
    mutate(
      gis_lat = latitude,
      gis_lon = longitude,
      meta_lat = latitude,
      meta_lon = longitude
    ) |>
    select(
      metastop_id,
      meta_lat,
      meta_lon,
      n_meta,
      gis_lat,
      gis_lon
    )

  stops <- left_join(data, stops, join_by(metastop_id))

  stops |>
    arrange(metastop_id, timestamp) |>
    group_by(metastop_id) |>
    filter(row_number() == 1) |>
    mutate(metastop = 10) |>
    rename(metastart_time = start_time, metaend_time = end_time) |>
    select(
      animal_id,
      metastop,
      n_meta,
      metastart_time,
      metaend_time,
      metastop_id,
      metastop_hours,
      metastop_days,
      meta_lat,
      meta_lon
    )
}

tidy_metastop_data <- function(data) {
  metastops <- data |>
    filter(metastop == 10) |>
    arrange(animal_id, metastart_time) |>
    group_by(metastop_id) |>
    filter(row_number() == 1) |>
    ungroup() |>
    mutate(
      stopover = 3,
      stop_id = metastop_id,
      gis_lat = meta_lat,
      gis_lon = meta_lon,
      stop_hours = metastop_hours,
      stop_days = metastop_days,
      n_locs = n_meta,
      timestamp = metastart_time,
      start_time = metastart_time,
      end_time = metaend_time
    ) |>
    select(
      -n_meta,
      -metastart_time,
      -metaend_time,
      -metastop_id,
      -metastop_hours,
      -metastop_days,
      -meta_lat,
      -meta_lon
    )

  non_metastops <- data |>
    select(
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
    mutate(stopover = metastop + stopover) |>
    filter(stopover != 11) |>
    mutate(
      original_lat = ifelse(stopover == 13, NA, original_lat),
      original_lon = ifelse(stopover == 13, NA, original_lon),
      lc = ifelse(stopover == 13, NA, lc)
    ) |>
    select(
      animal_id,
      timestamp,
      start_time,
      end_time,
      stop_id,
      stop_days,
      stopover,
      original_lat,
      original_lon,
      gis_lat,
      gis_lon,
      n_locs,
      lc,
      -stop_hours,
      -metastop
    ) |>
    arrange(animal_id, timestamp, desc(stopover)) |>
    mutate(locType = stopover_to_label(stopover))
}

# Prepare data for leaflet map by using GIS lat/lon where necessary
# and building aesthetic variables.
data_for_leaflet <- function(data) {
  data <- data |>
    mutate(
      animal_id = as.factor(animal_id),
      myRadius = stopover_to_radius(stopover),
      latitude = ifelse(is.na(original_lat), gis_lat, original_lat),
      longitude = ifelse(is.na(original_lon), gis_lon, original_lon),
      n_stops = n_locs,
      myRadius = if_else(stopover == 13, ((n_stops / 10 * myRadius) + myRadius), myRadius)
    ) |>
    select(animal_id, timestamp, latitude, longitude, lc, stop_days, locType, myRadius, stop_id, n_stops) |>
    ungroup()

  # Validate and filter coordinates
  data <- data |>
    filter(latitude >= -90 & latitude <= 90) |>
    filter(longitude >= -180 & longitude <= 360)

  # Sort by animal_id and timestamp
  data <- data |>
    arrange(animal_id, timestamp)

  data
}

# Placeholder data frames for use when no stops/metastops are identified. By
# default, when no stops are identified the stop identification algorithm
# fails.
#
# We use these to instead augment the raw location data to force all locations
# to be classified as movement locations. This prevents the app from crashing
# while maintaining an intuitive output result.
mutate_empty_stops <- function(data) {
  data |>
    mutate(
      start_time = NA,
      end_time = NA,
      stop_id = NA,
      stop_hours = NA,
      stop_days = NA,
      stopover = 0,
      original_lat = latitude,
      original_lon = longitude,
      gis_lat = latitude,
      gis_lon = longitude,
      stopover_lat = NA,
      stopover_lon = NA,
      n_locs = NA
    ) |>
    select(-latitude, -longitude)
}

mutate_empty_metastops <- function(data) {
  data |>
    mutate(locType = stopover_to_label(stopover)) |>
    select(-c(stop_hours, stopover_lat, stopover_lon))
}

# Helpers to recode vectors based on a key/value dictionary
lc_recode <- function(x) {
  key_recode(x, lc_weights(), 1)
}

stopover_to_radius <- function(x) {
  key_recode(x, stopover_radii(), 99)
}

stopover_to_label <- function(x) {
  key_recode(x, stopover_labels(), "Unclassified")
}

# Key/value dictionaries containing mappings between recodes for relevant
# variables
lc_weights <- function() {
  c(
    "G" = 5000,
    # "4" = 5000, # This is in original code, but is not listed as a valid LC val
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
    "13" = "Metastop",
    "12" = "Stopped",
    "10" = "Movement during stop",
    "2"  = "Movement",
    "0"  = "Movement"
  )
}

key_recode <- function(x, key, replace = NA) {
  y <- unname(key[as.character(x)])
  y[is.na(y)] <- replace
  y
}

has_argos_lc <- function(data) {
  "argos_lc" %in% colnames(data)
}

# Handle dateline crossing. When tracks cross the dateline, update
# longitude values accordingly to wrap the dateline.
get_elon <- function(lon, dateline = FALSE) {
  if (dateline) {
    ifelse(lon < 0, lon + 360, lon)
  } else {
    lon
  }
}
