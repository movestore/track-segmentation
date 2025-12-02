
prep_stops_output <- function(data) {
  data |> 
    dplyr::filter(stopover == 1) |>
    dplyr::rename(latitude = stopover_lat, longitude = stopover_lon) |>
    dplyr::mutate(locType = "Stopover") |>
    dplyr::select(
      animal_id,
      species,
      start_time,
      end_time,
      stop_hours,
      latitude,
      longitude,
      locType,
      n_locs,
      stop_id
    ) |>
    na.omit()
}

# ready4 input
prep_metastops_output <- function(data) {
  data |> 
    filter(stopover == 13) |>
    select(
      animal_id,
      species,
      start_time,
      end_time,
      stop_days,
      latitude = gis_lat,
      longitude = gis_lon,
      locType,
      n_stops = n_locs,
      meta_stop_id = stop_id
    )
}

prep_location_output <- function(data1, data2) {
  temp1 <- data2 |> 
    mutate(metastart_time = start_time, metaend_time  = end_time) |> 
    select(animal_id, n_stops, metastart_time, metaend_time, meta_stop_id)
  
  newLocData <- data1 |> 
    filter(stopover != 13) |> 
    left_join(
      temp1, 
      join_by(animal_id, timestamp >= metastart_time,timestamp <= metaend_time)
    ) |> 
    select(
      animal_id, 
      species, 
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

write_results <- function(stops,
                          metastops,
                          proximity,
                          min_hours,
                          version = app_version()) {
  fnames <- lapply(
    c("stopovers", "metaStops", "locationsAnnotated"),
    function(x) {
      out_file_name(x, proximity, min_hours, version = version)
    }
  )
  
  fname_zip  <- out_file_name(
    "track_segmentation", 
    proximity, 
    min_hours, 
    version = version, 
    ext = "zip"
  )
  
  tmp <- tempdir()
  out_dir <- file.path(tmp, "track-segmentation")
  
  if (dir.exists(out_dir)) {
    unlink(list.files(out_dir, full.names = TRUE, recursive = TRUE))
  } else {
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
  
  # TODO: Is `appArtifactPath()` supposed to construct file path itself?
  # currently just concatenates filename with the artifact path...
  # This seems to conflict with MoveApps docs example for shiny output files
  zip_file <- moveapps::appArtifactPath(paste0("/", fname_zip))
  zip::zip(zip_file, files = unlist(files), mode = "cherry-pick")
  
  zip_file
}

app_version <- function() {
  "014"
}

sanitize_studyName <- function(studyName) {
  # Invalid Windows filename characters...
  invalid_chars <- "[<>:\"/\\\\|?*]"
  # Replace invalid characters with an underscore...
  cleaned_studyName <- gsub(invalid_chars, "_", studyName)
  return(cleaned_studyName)
}

