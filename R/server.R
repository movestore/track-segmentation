
# data -> raw seg data
id_stops <- function(data, min_hours, proximity) {
  data2 <- find_periods_move2(
    data,
    start_idx = 1,
    min_hours = min_hours,
    proximity = proximity 
  )
  
  # isolote and annotate raw locations that were not stopped....
  transit_locs <- data2 |>
    dplyr::filter(is.na(stop_id)) |>
    dplyr::mutate(
      stopover = 0,
      original_lat = latitude, # Still not clear why we need all these different records of lat/lon
      original_lon = longitude,
      gis_lat = latitude, 
      gis_lon = longitude
    ) |> 
    dplyr::select(-latitude, -longitude)
  
  # isolate and annotate the raw locations that were stopped
  stopped_locs <- data2 |>
    dplyr::filter(!is.na(stop_id)) |>
    dplyr::mutate(
      stopover = 2,
      original_lat = latitude,
      original_lon = longitude,
      gis_lat = original_lat,
      gis_lon = original_lon,
      w = lc_recode(lc)
    ) |> 
    dplyr::select(-latitude, -longitude)
  
  weighted_stopped_locs <- stopped_locs |> 
    dplyr::mutate(
      stopover_lat = weighted_lat(original_lat, w),
      stopover_lon = weighted_lon(original_lon, w),
      n_locs = dplyr::n(),
      .by = stop_id
    ) |> 
    dplyr::filter(timestamp == min(timestamp), .by = stop_id) |>
    dplyr::mutate(stopover = 1,
                  lc = NA,
                  original_lat = NA, 
                  original_lon = NA,
                  gis_lat = NA,
                  gis_lon = NA)
  
  Dateline <- FALSE
  
  stops_data <- dplyr::bind_rows(
    transit_locs, 
    stopped_locs, 
    weighted_stopped_locs
  ) |>
    dplyr::select(-w) |>
    arrange(animal_id, timestamp) |>
    mutate(gis_elon = ifelse(gis_lon < 0 & Dateline, gis_lon + 360, gis_lon))
  
  stops_data
}

# data -> "tier5"
id_metastops <- function(data, min_hours, proximity) {
  meta <- stops_to_metastops(data)
  
  meta2 <- find_periods_move2(
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
  
  meta3 <- get_metastop_stats(meta2)
  
  ready <- data |>
    left_join(
      meta3, 
      join_by(animal_id, timestamp >= metastart_time,timestamp <= metaend_time)
    ) |>
    mutate(metastop = replace_na(metastop, 0))
  
  tidy_metastop_data(ready)
}

mutate_empty_stops <- function(data) {
  dateline <- FALSE
  
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
      n_locs = NA,
      gis_elon = ifelse(gis_lon < 0 & dateline, gis_lon + 360, gis_lon)
    ) |> 
    select(-latitude, -longitude)
}

mutate_empty_metastops <- function(data) {
  data |> 
    mutate(
      locType = stopover_to_label(stopover)
    ) |> 
    select(
      -c(stop_hours, original_lat, original_lon, stopover_lat, stopover_lon)
    )
}
