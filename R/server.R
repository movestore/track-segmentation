# Run the segmentation algorithm with a set proximity and duration to identify
# stop locations. This wraps `identify_stops()` (which actually runs the
# stop location identification process) and annotates raw locations with
# their stop information, if any. It also calculates the weighted location of
# each stop. Input data should be the raw seg data as produced by
# `move2_to_seg()`
find_stop_locations <- function(data, min_hours, proximity, dateline = FALSE) {
  data2 <- identify_stops(
    data,
    start_idx = 1,
    min_hours = min_hours,
    proximity = proximity
  )

  # isolote and annotate raw locations that were not stopped
  transit_locs <- data2 |>
    dplyr::filter(is.na(stop_id)) |>
    dplyr::mutate(
      stopover = 0,
      original_lat = latitude,
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

  # Calculate weighted stop locations
  weighted_stopped_locs <- stopped_locs |>
    dplyr::mutate(
      stopover_lat = weighted_lat(original_lat, w),
      stopover_lon = weighted_lon(original_lon, w),
      n_locs = dplyr::n(),
      .by = stop_id
    ) |>
    dplyr::filter(timestamp == min(timestamp), .by = stop_id) |>
    dplyr::mutate(
      stopover = 1,
      lc = NA,
      original_lat = NA,
      original_lon = NA,
      gis_lat = NA,
      gis_lon = NA
    )

  # Recombine raw transit locations with annotated stop locations and
  # weighted stop locations
  stops_data <- dplyr::bind_rows(
    transit_locs,
    stopped_locs,
    weighted_stopped_locs
  ) |>
    dplyr::select(-w) |>
    arrange(animal_id, timestamp) |>
    mutate(gis_elon = get_elon(gis_lon, dateline))

  stops_data
}

# Run the segmentation algorithm with a set proximity and duration to identify
# metastop locations. This prepares the output of the stop identification
# step to be passed back through the segmentation algorithm (`identify_stops()`)
# Because the results of the stop identification include annotated observed
# location data, but we want to re-run the algorithm only on the weighted
# stop locations themselves, we must massage the data before passing back through
# the algorithm. Input data should be the results of `find_stop_locations()`
find_metastop_locations <- function(data, min_hours, proximity, dateline = FALSE) {
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

  tidy_metastop_data(meta, dateline = dateline)
}

# Placeholder data frames for use when no stops/metastops are identified. By
# default, when no stops are identified the stop identification algorithm
# fails.
#
# We use these to instead augment the raw location data to force all locations
# to be classified as movement locations. This prevents the app from crashing
# while maintaining an intuitive output result.
mutate_empty_stops <- function(data, dateline = FALSE) {
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
      gis_elon = get_elon(gis_lon, dateline)
    ) |>
    select(-latitude, -longitude)
}

mutate_empty_metastops <- function(data) {
  data |>
    mutate(locType = stopover_to_label(stopover)) |>
    select(-c(stop_hours, stopover_lat, stopover_lon))
}
