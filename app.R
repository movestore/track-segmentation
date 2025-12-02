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
  has_stops <- reactiveVal(FALSE)
  has_metastops <- reactiveVal(FALSE)

  # Ensure busy spinner starts before data prep, since both depend on recalc
  # button event
  observeEvent(input$recalc, priority = 100, {
    shinybusy::show_modal_spinner("radar")
  })
  
  find_stops <- eventReactive(input$recalc, {
    stops <- id_stops(data, input$min_hours, input$proximity)
    
    has_stops(TRUE)
    
    list(
      result = stops,
      min_hours = isolate(input$min_hours),
      proximity = isolate(input$proximity)
    )
  })
  
  find_metastops <- eventReactive(find_stops(), {
    stops <- find_stops()
    metastops <- id_metastops(stops$result, stops$min_hours, stops$proximity)
    
    has_metastops(TRUE)
    
    metastops
  })
  
  prep_map_data <- reactive({
    data_for_leaflet(find_metastops())
  })
  
  # Create the initial base map...
  output$map <- renderLeaflet({
    create_base_map(bbox)
  })
  
  # Update tracking data when time range changes...
  observe({
    res <- prep_map_data()
    
    req(res)
    req(input$timeRange)
    
    leafletProxy("map") |>
      clearGroup(group = unique(res$animal_id)) |>
      add_tracking_data(res, input$timeRange)
    
    shinybusy::remove_modal_spinner()
  })
  
  output$data_contents <- renderUI({
    tagList(
      h3("Stops"),
      DT::dataTableOutput("stop_data"),
      hr(),
      h3("Metastops"),
      DT::dataTableOutput("metastop_data")
    )
  })
  
  output$data_overlay <- renderUI({
    if (!has_stops() || !has_metastops()) {
      div(
        style = overlay_style(),
        "Run the analysis to view data in this tab."
      )
    }
  })
  
  output$stop_data <- DT::renderDataTable({
    prep_stops_output(find_stops()$result)
  })
  
  output$metastop_data <- DT::renderDataTable({
    prep_metastops_output(find_metastops())
  })
}

shinyApp(ui, server)
