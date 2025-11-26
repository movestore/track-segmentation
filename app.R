library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(leaflet.extras)

source("R/ui.R")
source("R/server.R")

Dateline  <- FALSE # Do any tracks cross the -180/+180 longitude meridian, TRUE or FALSE?
frameRate <- 24 # Define an animation frame-rate for the data visualization, units=hours....
subsetAnimals <- FALSE

proximity_distance_meters <- 150 # Define the maximum pairwise distance (Dist) among all locations during a stop (units = meters)....
minimum_hours_threshold   <- 12 # Define the minimum duration (Dur) for a stopover (units = hours)....
max_days_threshold <- 365000  # Here set to 1000 years, so it has no influence on the results.

FAIL <- FALSE

studyName <- "example01"

# Data -----------

# This will ultimately come from previous MoveApp
data <- readRDS("~/Documents/projects/track-segmentation/data/raw/input1_move2loc_LatLon.rds")
# data <- read.csv("~/Documents/projects/track-segmentation-orig/inputData_example01.csv")

# Transform to WGS84
if (sf::st_crs(data) != sf::st_crs("epsg:4326")) {
  data <- sf::st_transform(data, "epsg:4326")
}

coords <- sf::st_coordinates(data) # likely could adapt code to use geometry col itself

move2::mt_track_id(data) <- "animal_id"
move2::mt_time(data) <- "timestamp"

data <- data |> 
  dplyr::mutate(
    animal_id = as.character(animal_id), # necessary?
    latitude = coords[, 2],
    longitude = coords[, 1],
    lc = "G", # Need to deal with this for ARGOS
    species = "misc"
  ) |> 
  dplyr::select(animal_id, timestamp, latitude, longitude, lc, species) |> 
  dplyr::arrange(move2::mt_track_id(data), move2::mt_time(data)) |> 
  na.omit() |> # necessary?
  dplyr::filter(dplyr::n() >= 2)

# Stopovers ------

check_data_frame(data)

# cleaned_studyName <- sanitize_studyName(studyName)
# 
# resultsPath <- paste0(resultsDir, cleaned_studyName, "/")
# 
# # Check if the directory exists...
# if (!dir.exists(resultsPath)) {
#   dir.create(resultsPath, recursive = TRUE, showWarnings = FALSE)
#   message("\nDirectory created: ", resultsPath, "\n")
# } else {
#   message("\nDirectory already exists: ", resultsPath, "\n")
# }

# Process each animal.... this is where the work gets done.....
results <- data |> 
  dplyr::group_by(animal_id) |>
  dplyr::group_split() |>
  lapply(function(animal_data) {
    find_periods_recursive(animal_data, 1, min_hours = minimum_hours_threshold, max_days = max_days_threshold)
  }) |>
  bind_rows()

# Stop the script if zero stops were detected.....
if (nrow(results) == 0) {
  FAIL <- TRUE
  stop(
    "\n\n>>>>EXITING SCRIPT: No stops were detected, try changing the Dist or Dur threshold, or both.", 
    call. = FALSE
  )
} 

# create a unique stop_ID variable....
results$stop_id <- paste0(format(results$start_time, "%Y%m%d%H%M%S"),'_',
                          format(results$end_time,   "%Y%m%d%H%M%S"),'_',results$animal_id)

n_raw_locations <- nrow(data)  # capture for a printed summary later....

# merge stops with input raw location records....
data2 <- data |>
  dplyr::left_join(
    results, 
    dplyr::join_by(animal_id, timestamp >= start_time, timestamp <= end_time)
  )

# derive some variables....
data2$duration <- difftime(data2$end_time, data2$start_time, units = "secs")
data2$stop_hours <- as.numeric(data2$duration/3600)
data2$stop_days  <- as.numeric(data2$duration/(3600*24))

data2 <- data2  %>%
  dplyr::select(-duration)

# isolote and annotate raw locations that were not stopped....
transit_locs <- data2 %>%
  subset(is.na(start_time)) %>%
  mutate(stopover = 0) %>%
  mutate(gis_lat = latitude, gis_lon = longitude) %>%
  mutate(original_lat = latitude, 
         original_lon = longitude)

# isolate and annotate the raw locations that were stopped
tier1 <- data2 %>%
  subset(!is.na(start_time)) %>%
  mutate(original_lat = latitude, 
         original_lon = longitude,
         stopover = 2)

# Temporary fix to remove move2 class to ensure subsquent code
# behaves as normal in original scripts. Will later update to accomodate move2
tier1 <- as_tibble(tier1)

# assign a Location Class weighting factor for calculating the
# single "weighted-average" location of the initial individual stopovers... 
tier1 <- tier1 %>%
  mutate(
    w = 
      ifelse(lc=='G', 5000, 
        ifelse(lc=='4', 5000, 
          ifelse(lc=='3', 1000, 
            ifelse(lc=='2',  500, 
              ifelse(lc=='1',  300, 
                ifelse(lc=='0',   50,
                  ifelse(lc=='A',  100,
                    ifelse(lc=='B',   10,
                      ifelse(lc=='Z',    5, 1)
                    )))))))))

tier2lon <- calculate_weighted_longitude(sf::st_drop_geometry(tier1))

# Second, latitude.....
tier2lat <- tier1 %>%
  sf::st_drop_geometry() |> 
  group_by(stop_id) %>%
  summarise(stopover_lat = weighted.mean(latitude,  w), 
            n_locs = n())

# Combine the lat,lon derivations....
tier2 <- 
  tier2lon %>%
  left_join(tier2lat, join_by(stop_id)) %>%
  select(stop_id,stopover_lat,stopover_lon,n_locs)

# merge the derived stopover location into the location records...
tier3 <- tier2 %>%
  left_join(tier1, join_by(stop_id)) 

# extract just one record per stopover...
tier4 <- tier3 %>%
  arrange(stop_id, timestamp) %>%
  group_by(stop_id) %>%
  filter(row_number() == 1) %>%
  mutate(stopover = 1, latitude = NA, longitude = NA, lc = NA,
         original_lat = NA, original_lon = NA)

n_initial_stops <- nrow(tier4)

# prep the locations associated with the stopovers...
tier1 <- tier1 %>%
  mutate(gis_lat = original_lat, 
         gis_lon = original_lon) %>%
  dplyr::select(-latitude, -longitude)

# combine with all of the data records: 
# transit_locs = non-stopped locs, 
# tier1 = stopped raw locs, and 
# tier4 = stopover derived locs...
transit_locs <- tibble::as_tibble(transit_locs) # remove move2; temporary
tier5 <- bind_rows(transit_locs,tier1,tier4) %>%
  arrange(animal_id, timestamp) %>%
  dplyr::select(-w, -latitude, -longitude) %>%
  mutate(gis_elon = 
           ifelse(
             gis_lon < 0 & Dateline, gis_lon + 360, 
             gis_lon
           ))

# stopovers4output <- tier5 %>%
#   subset(stopover == 1) %>%
#   rename(latitude=stopover_lat,longitude=stopover_lon) %>%
#   dplyr::mutate(locType = 'Stopover') %>%
#   dplyr::select(animal_id,species,start_time,end_time,stop_hours,latitude,longitude,
#                 locType,n_locs,stop_id) %>%
#   na.omit()

# First, extract just the weighted-mean stopover locations, cross-walk key variables....
# This set using start_time...
meta0a <- tier5 %>%
  subset(stopover == 1) %>%
  rename(latitude=stopover_lat,longitude=stopover_lon) %>%
  mutate(timestamp=start_time) %>%
  dplyr::select(animal_id,timestamp,latitude,longitude,
                species,n_locs) %>%
  na.omit()
# Second, extract just the weighted-mean stopover locations, cross-walk key variables....
# This set using end_time...
meta0b <- tier5 %>%
  subset(stopover == 1) %>%
  rename(latitude=stopover_lat,longitude=stopover_lon) %>%
  mutate(timestamp=end_time) %>%
  dplyr::select(animal_id,timestamp,latitude,longitude,
                species,n_locs) %>%
  na.omit()

# Combined the 2 sets of stops (one with timestamp=start_time and the other with timestamp=end_time)
meta0 <- rbind(meta0a, meta0b)

# Sort data by animal_id and timestamp.... 
# need more than one stop to consider existence of a metastop....
meta <- meta0 %>%
  arrange(animal_id, timestamp) %>%
  group_by(animal_id) %>%
  filter(n() >= 2)

# nrow(meta0)-nrow(meta)
resultsMeta <- meta %>%
  group_by(animal_id) %>%
  group_split() %>%
  lapply(function(animal_data) {
    find_periods_recursive(animal_data, 1, min_hours = minimum_hours_threshold, max_days = max_days_threshold)
  }) %>%
  bind_rows()

n_meta_stops <- nrow(resultsMeta)

# create a unique metastop_ID variable....
resultsMeta$metastop_id <- paste0(format(resultsMeta$start_time, "%Y%m%d%H%M%S"),'_',
                                  format(resultsMeta$end_time, "%Y%m%d%H%M%S"),'_',resultsMeta$animal_id)

# merge metastops with input stopover records....
meta2 <- 
  meta %>%
  left_join(resultsMeta, join_by(animal_id, timestamp >= start_time,timestamp <= end_time)) 

meta2$metaduration <- difftime(meta2$end_time, meta2$start_time, units = "secs")
meta2$metastop_hours <- as.numeric(meta2$metaduration/3600)
meta2$metastop_days  <- as.numeric(meta2$metaduration/(3600*24))

meta2 <- meta2  %>%
  dplyr::select(-metaduration)

# isolate the detected metastops.... 
tier1meta <- meta2 %>%
  subset(!is.na(start_time)) 

# DO NOT average the stopover locations associated with each metastop.
# INSTEAD, USE THE STOPOVER WITH THE LARGEST NUMBER OF RAW LOCATIONS
tier2meta <- tier1meta %>%
  group_by(metastop_id) %>%
  mutate(n_meta = n()/2) %>%
  arrange(metastop_id, desc(n_locs)) %>%
  filter(row_number() == 1) %>%
  mutate(gis_lat = latitude, gis_lon = longitude) %>%
  mutate(meta_lat = latitude, meta_lon = longitude) %>%
  dplyr::select(metastop_id,meta_lat,meta_lon,n_meta,gis_lat,gis_lon)

# merge the picked metastop location with the meta stop data....
tier3meta <- tier1meta %>%
  left_join(tier2meta, join_by(metastop_id)) 

# extract 1 record per metastop....
tier4meta <- tier3meta %>%
  arrange(metastop_id, timestamp) %>%
  group_by(metastop_id) %>%
  filter(row_number() == 1) %>%
  mutate(metastop = 10) %>%
  rename(metastart_time=start_time,metaend_time=end_time) %>%
  dplyr::select(animal_id,metastop,n_meta,metastart_time,metaend_time,
                metastop_id,metastop_hours,metastop_days,
                meta_lat,meta_lon)

# merge the metastops into the fullmonty data set of just single stops....
tier5$metastop <- 0

ready <- 
  tier5 %>%
  left_join(tier4meta, join_by(animal_id, timestamp >= metastart_time,timestamp <= metaend_time)) %>%
  mutate(metastop.x = replace_na(metastop.x,0), metastop.y = replace_na(metastop.y,0)) %>%
  mutate(metastop = metastop.x + metastop.y) %>%
  dplyr::select(-metastop.x, -metastop.y)

# isolate just the metastop records.... coerce consistent variable names for the metastops....
justMeta <- ready %>%
  subset(metastop == 10) %>%
  arrange(animal_id,metastart_time) %>%
  group_by(metastop_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(stopover = 3, stop_id = metastop_id, gis_lat = meta_lat, gis_lon = meta_lon,
         stop_hours = metastop_hours, stop_days = metastop_days,
         n_locs = n_meta, timestamp = metastart_time,
         start_time = metastart_time, end_time = metaend_time) %>%
  dplyr::select(-n_meta,-metastart_time,-metaend_time,-metastop_id,-metastop_hours,-metastop_days,
                -meta_lat,-meta_lon)

# drop the metastop variables....
readyX <- ready %>%
  dplyr::select(-n_meta,-metastart_time,-metaend_time,-metastop_id,-metastop_hours,-metastop_days,
                -meta_lat,-meta_lon)

# add the summarized metastop records to the fullmonty....
ready2 <- rbind(readyX, justMeta)

# sort...
# classify stopover location type...
# calc east longitude value...
ready3 <- ready2 %>%
  arrange(animal_id,timestamp) %>%
  mutate(stopover2 = metastop + stopover) %>%
  mutate(gis_elon = 
           ifelse(
             gis_lon < 0 & Dateline, gis_lon + 360, 
             gis_lon
           )) 

ready4 <- ready3 %>%
  subset(stopover2 != 11) %>%
  dplyr::select(-stop_hours,-stopover,-metastop) %>%
  rename(stopover = stopover2) %>%
  mutate(original_lat = ifelse(stopover == 13, NA, original_lat)) %>%
  mutate(original_lon = ifelse(stopover == 13, NA, original_lon)) %>%
  mutate(lc = ifelse(stopover == 13, NA, lc)) %>%
  dplyr::select(animal_id,timestamp,start_time,end_time,
                stop_id,stop_days,stopover,gis_lat,gis_lon,gis_elon,
                n_locs,lc,species) %>%
  arrange(animal_id,timestamp,desc(stopover))


if (exists("frameRate") && is.numeric(frameRate) && length(frameRate) == 1) {
  definedSeed <- frameRate * 3600
} else {
  definedSeed <- 86400  # default to 1-day frame rate....
}

# Stop the script if zero stops were detected.....
if (FAIL) {
  stop("\n>>>>No stops, so no visualization.", 
       call. = FALSE)
}

# First update the data preparation....
prepOut     <- prepare_data()
dataLeaflet <- prepOut$dataLeaflet
lc_colors   <- prepOut$lc_colors
pal         <- prepOut$palette

# Now update the UI object....
uiOut <- build_ui()
ui    <- uiOut$ui

# ------------------------------------------------------------------------------

ui <- fluidPage(
  ui
)

server <- function(input, output, session) {
  # Create the initial base map...
  output$map <- renderLeaflet({
    create_base_map()
  })
  
  # Update tracking data when time range changes...
  observe({
    req(input$timeRange)
    leafletProxy("map") %>%
      clearGroup(group = unique(dataLeaflet$animal_id)) %>%
      add_tracking_data(dataLeaflet, input$timeRange) 
  })
}

shinyApp(ui, server)
