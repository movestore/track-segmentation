# Reformat stop results data for display and output CSV file
prep_stops_output <- function(data) {
  if (nrow(data) == 0) {
    return(dplyr::tibble())
  }

  data |>
    filter(stopover == 1) |>
    rename(latitude = stopover_lat, longitude = stopover_lon) |>
    mutate(locType = "Stopover") |>
    select(
      animal_id,
      stop_id,
      start_time,
      end_time,
      latitude,
      longitude,
      stop_hours,
      n_locs
    ) |>
    na.omit()
}

# Reformat metastop results data for display and output CSV file
prep_metastops_output <- function(data) {
  if (nrow(data) == 0) {
    return(dplyr::tibble())
  }

  data |>
    filter(stopover == 13) |>
    select(
      animal_id,
      meta_stop_id = stop_id,
      start_time,
      end_time,
      latitude = gis_lat,
      longitude = gis_lon,
      stop_days,
      n_stops = n_locs
    )
}

# Reformat location results data for output CSV file. data1 should be
# the results of the metastops segmentation process. data2 should be the
# metastops data already prepared for output. Both arguments are exposed
# to prevent need to prep metastops output twice. They will be rejoined
# to link metastops to transit locations
prep_location_output <- function(data1, data2) {
  if (nrow(data1) == 0 || nrow(data2) == 0) {
    return(dplyr::tibble())
  }

  temp1 <- data2 |>
    mutate(metastart_time = start_time, metaend_time = end_time) |>
    select(animal_id, n_stops, metastart_time, metaend_time, meta_stop_id)

  # Get non-metastop locations and join back on metastops results to link
  # original transit locations to associated metastops
  newLocData <- data1 |>
    filter(stopover != 13) |>
    left_join(
      temp1,
      join_by(animal_id, timestamp >= metastart_time, timestamp <= metaend_time)
    ) |>
    select(
      animal_id,
      timestamp,
      latitude = gis_lat,
      longitude = gis_lon,
      lc,
      locType,
      stop_id,
      n_stops,
      meta_stop_id
    )

  newLocData
}

out_file_name <- function(prefix,
                          proximity,
                          min_hours,
                          ext = "csv",
                          version = app_version()) {
  paste0(
    prefix,
    "_v", version,
    "_proxMeters", proximity,
    "_minHours", min_hours,
    ".", ext
  )
}

# Write a zip file containing stops, metastops, and transit location output
write_results <- function(stops,
                          metastops,
                          proximity,
                          min_hours,
                          version = app_version()) {
  fnames <- lapply(
    c("stopovers", "metastops", "locationsAnnotated"),
    function(x) {
      out_file_name(x, proximity, min_hours, version = version)
    }
  )

  fname_zip <- out_file_name(
    "track_segmentation",
    proximity,
    min_hours,
    version = version,
    ext = "zip"
  )

  tmp <- tempdir()
  out_dir <- file.path(tmp, "track-segmentation")

  logger.debug(paste0("Setting write dir: ", out_dir))
  
  if (dir.exists(out_dir)) {
    logger.debug(paste0("Writing files to tempdir: ", out_dir))
    unlink(list.files(out_dir, full.names = TRUE, recursive = TRUE))
  } else {
    logger.debug(paste0("Dir does not exist. Existing dirs: ", list.files(tmp)))
    dir.create(out_dir)
  }

  files <- list(
    file.path(out_dir, fnames[[1]]),
    file.path(out_dir, fnames[[2]]),
    file.path(out_dir, fnames[[3]])
  )

  stops_to_write <- prep_stops_output(stops)
  metastops_to_write <- prep_metastops_output(metastops)

  # `metastops` actually contains all movement locations, it's only the prepped
  # metastops output that is filtered. Pass both to prevent recalculation
  # of `metastops_to_write`, though this is admittedly a strange function
  # construction.
  transit_to_write <- prep_location_output(metastops, metastops_to_write)

  write.csv(stops_to_write, files[[1]], row.names = FALSE)
  write.csv(metastops_to_write, files[[2]], row.names = FALSE)
  write.csv(transit_to_write, files[[3]], row.names = FALSE)

  logger.debug(paste0("Writing zip to ", moveapps::appArtifactPath(fname_zip)))
  zip_file <- moveapps::appArtifactPath(fname_zip)
  zip::zip(zip_file, files = unlist(files), mode = "cherry-pick")

  zip_file
}

app_version <- function() {
  "014"
}

# Improve formatting for output tables in app Data tab to make tables more
# legible. Doesn't affect written output tables.
prettify <- function(data, digits = 6) {
  mutate(
    data,
    across(where(is.numeric), ~ round(.x, digits)),
    across(where(~ inherits(.x, "POSIXct")), ~ format(.x, "%Y-%m-%d %H:%M:%S %Z"))
  )
}
