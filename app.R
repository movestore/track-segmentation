library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(leaflet.extras)

source("R/ui.R")
source("R/server.R")
source("R/segmentation.R")
source("R/output.R")
source("R/map.R")
source("R/server.R")

study_name <- "my_test_study"
pal <- stopover_pal()

# This will ultimately come from previous MoveApp
data_raw <- readRDS("~/Documents/projects/track-segmentation/data/raw/input2_move2loc_LatLon.rds")
# data_raw <- move2::movebank_download_study(study_id = 438644854, sensor_type_id = "argos-doppler-shift")

bbox <- sf::st_bbox(data_raw)

data <- move2_to_seg(data_raw)

check_data_frame(data)

# ------------------------------------------------------------------------------

ui <- fluidPage(
  seg_ui(data = data_raw, min_hours = min_hours, proximity = proximity, step = 86400)
)

server <- function(input, output, session) {
  stop_data <- eventReactive(input$recalc, {
    shinybusy::show_modal_spinner("radar")
    
    stops <- calculate_stops(
      data, 
      min_hours = input$min_hours, 
      proximity = input$proximity
    )
    
    shinybusy::remove_modal_spinner()
    
    stops
  })
  
  # Create the initial base map...
  output$map <- renderLeaflet({
    create_base_map(bbox)
  })
  
  # Update tracking data when time range changes...
  observe({
    req(input$timeRange)
    
    res <- stop_data()
    req(res)
    
    leafletProxy("map") |>
      clearGroup(group = unique(res$animal_id)) |>
      add_tracking_data(res, input$timeRange)
  })
}

shinyApp(ui, server)
