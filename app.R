library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(leaflet.extras)

source("R/ui.R")
source("R/server.R")
source("R/segmentation.R")
source("R/output.R")

min_hours <- 12
proximity <- 150
study_name <- "my_test_study"

# This will ultimately come from previous MoveApp
data <- readRDS("~/Documents/projects/track-segmentation/data/raw/input1_move2loc_LatLon.rds")

# Can still generalize to move2 more, right now this still hard codes various columns.
data <- move2_to_seg(data) |> mutate(species = "misc") # species is placeholder for now

check_data_frame(data)

# Is this taking way longer than other version? Can't reproduce but seemed to be running faster...
data2 <- find_periods_move2(
  data,
  start_idx = 1,
  min_hours = min_hours,
  proximity = proximity 
)

# isolote and annotate raw locations that were not stopped....
transit_locs <- data2 %>%
  subset(is.na(start_time)) %>%
  mutate(stopover = 0) %>%
  mutate(gis_lat = latitude, gis_lon = longitude) %>%
  mutate(original_lat = latitude, original_lon = longitude)

# isolate and annotate the raw locations that were stopped
tier1 <- data2 %>%
  subset(!is.na(start_time)) %>%
  mutate(
    original_lat = latitude, 
    original_lon = longitude,
    stopover = 2,
    w = lc_recode(lc)
  ) |> 
  tibble::as_tibble() # Temporary fix to remove move2 class to ensure subsquent code

tier4 <- calculate_weighted_loc(tier1) |>
  dplyr::left_join(tier1, dplyr::join_by(stop_id)) |>
  dplyr::filter(timestamp == min(timestamp), .by = stop_id) |>
  mutate(stopover = 1, latitude = NA, longitude = NA, lc = NA,
         original_lat = NA, original_lon = NA)

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

Dateline <- FALSE

tier5 <- bind_rows(transit_locs,tier1,tier4) %>%
  arrange(animal_id, timestamp) %>%
  dplyr::select(-w, -latitude, -longitude) %>%
  mutate(gis_elon = 
           ifelse(
             gis_lon < 0 & Dateline, gis_lon + 360, 
             gis_lon
           ))

# For output ---------

stops_output <- prep_stops_output(tier5)

# --------------------

meta <- stops_to_metastops(tier5)

meta2 <- find_periods_move2(
  meta, 
  start_idx = 1, 
  min_hours = min_hours, 
  proximity = proximity
) |> 
  rename(metastop_id = stop_id, metastop_hours = stop_hours, metastop_days = stop_days)

tier4meta <- get_metastop_loc(meta2)

ready <- tier5 %>%
  # TODO: Do we really need to rejoin all of these things? Can't we just add this as a layer in the output map?
  left_join(tier4meta, join_by(animal_id, timestamp >= metastart_time,timestamp <= metaend_time)) %>%
  mutate(metastop = replace_na(metastop, 0))

ready4 <- tidy_metastop_data(ready)
ready4 <- ready4 |> mutate(locType = stopover_to_label(stopover))

# For output ----------

metastop_output <- prep_metastops_output(ready4)
location_output <- prep_location_output(ready4, metastop_output)

# ---------------------

# Write ---------------

# TODO This could be a separate tab panel in the app and there could
# be a button to export? Not sure how app triggered export works in MoveApps Shiny
# exactly
write_results(
  location_output, 
  stops_output, 
  metastop_output, 
  proximity = proximity, 
  min_hours = min_hours, 
  study_name = study_name
)

# ---------------------

# First update the data preparation....
dataLeaflet <- data_for_leaflet(ready4)
lc_colors <- lc_colors()
pal <- stopover_pal()

# ------------------------------------------------------------------------------

ui <- fluidPage(
  ui2(data = dataLeaflet, min_hours = min_hours, proximity = proximity, step = 86400)
)

server <- function(input, output, session) {
  # Create the initial base map...
  output$map <- renderLeaflet({
    create_base_map(sf::st_bbox(sf::st_as_sf(dataLeaflet, coords = c("longitude", "latitude"))))
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
