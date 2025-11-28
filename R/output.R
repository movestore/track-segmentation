
prep_stops_output <- function(data) {
  data |> 
    subset(stopover == 1) %>%
    rename(latitude = stopover_lat, longitude = stopover_lon) %>%
    dplyr::mutate(locType = "Stopover") %>%
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
    ) %>%
    na.omit()
}

# ready4 input
prep_metastops_output <- function(data) {
  data |> 
    filter(stopover == 13) %>%
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

# data1 = ready4? data2 = prep_metastops_output() output...?
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

out_file_name <- function(what, 
                          proximity, 
                          min_hours, 
                          study_name, 
                          ext = "csv",
                          version = app_version()) {
  paste0(
    what, 
    "_v", version,
    "_proxMeters_", proximity,
    "_minHours", min_hours,
    "_", study_name,
    ".", ext
  )
}

write_results <- function(transit, stops, metastops, proximity, min_hours, study_name, version = app_version()) {
  fname_stop <- out_file_name("stopovers", proximity, min_hours, study_name, version = version)
  fname_meta <- out_file_name("metaStops", proximity, min_hours, study_name, version = version)
  fname_move <- out_file_name("locationsAnnotated", proximity, min_hours, study_name, version = version)
  fname_zip  <- out_file_name("track_segmentation", proximity, min_hours, study_name, version = version, ext = "zip")
  
  tmp <- tempdir()
  
  dir.create(tmp)
  
  files <- list(
    file.path(tmp, fname_stop),
    file.path(tmp, fname_meta),
    file.path(tmp, fname_move)
  )
  
  write.csv(stops, files[[1]])
  write.csv(metastops, files[[2]])
  write.csv(transit, files[[3]])
  
  zip_file <- moveapps::appArtifactPath(fname_zip)
  
  zip::zip(zip_file, files = unlist(files), mode = "cherry-pick")
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

