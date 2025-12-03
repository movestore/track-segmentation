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

# This redirects output to tempdir for dev purposes
# TODO: after adapting to MoveApps framework, output location should be 
# handled by .env instead
Sys.setenv("APP_ARTIFACTS_DIR" = tempdir())

# This will ultimately come from previous MoveApp
data_raw <- readRDS("~/Documents/projects/track-segmentation/data/raw/input2_move2loc_LatLon.rds")
# data_raw <- move2::movebank_download_study(study_id = 438644854, sensor_type_id = "argos-doppler-shift")

bbox <- sf::st_bbox(data_raw)

data <- move2_to_seg(data_raw)

check_data_frame(data)

# ------------------------------------------------------------------------------

ui <- fluidPage(
  seg_ui(data = data_raw, min_hours = 6, proximity = 150, step = 86400)
)

server <- function(input, output, session) {
  has_stops <- reactiveVal(FALSE)
  has_metastops <- reactiveVal(FALSE)
  results_zip <- reactiveVal(NULL)
  map_data <- reactiveVal(list(type = "init", data = data))
  map_trigger <- reactiveVal(0)
  button_invalid <- reactiveVal(TRUE)
  
  filt_data <- reactive({
    d <- map_data()$data
    req(d)
    
    dplyr::filter(
      d, 
      timestamp >= input$timeRange[1], 
      timestamp <= input$timeRange[2]
    )
  })
  
  # Ensure busy spinner starts before data prep, since both depend on recalc
  # button event
  observeEvent(input$recalc, priority = 100, {
    shinybusy::show_modal_spinner("radar")
  })
  
  # Track sliders, change button CSS to indicate when segmentation needs rerun
  observeEvent(list(input$min_hours, input$proximity), {
    button_invalid(TRUE)
  })
  
  observe({
    if (button_invalid()) {
      shinyjs::enable("recalc")
    } else {
      shinyjs::disable("recalc")
    }
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
    button_invalid(FALSE)
    map_data(list(type = "processed", data = data_for_leaflet(metastops)))
    
    map_trigger(map_trigger() + 1)
    
    metastops
  })
  
  observeEvent(find_metastops(), {
    stops <- find_stops()
    metastops <- find_metastops()
    
    # Remove existing results zip if it exists
    if (!is.null(results_zip())) {
      unlink(results_zip())
      results_zip(NULL)
    }
    
    f_out <- write_results(
      stops$result,
      metastops,
      stops$proximity,
      stops$min_hours
    )
    
    results_zip(f_out)
  })
  
  # Create the initial base map...
  output$map <- renderLeaflet({
    create_basemap(bbox)
  })
  
  # Update tracking data when time range changes...
  observe({
    # Dependency on map render counter ensures that we always re-render
    # map on input$recalc trigger, even if inputs don't change.
    # (We don't want to explicitly trigger this on input$recalc because
    # we need more control over temporal ordering of processing steps)
    map_trigger()
    
    data_status <- map_data()$type
    filt_map_data <- filt_data()
    
    req(filt_map_data)
    req(data_status)
    
    map <- leafletProxy("map") |> 
      clearGroup(group = unique(filt_map_data$animal_id)) |>
      addTrackLayersControl(filt_map_data) |> 
      addTrackLines(filt_map_data)
    
    # Initial data do not have all necessary attributes for coloring in the same
    # way as processed data. After the first time stops are calculated, we can
    # render with the stop/metastop styling
    if (data_status == "init") {
      map <- map |>
        addTrackLocationMarkers(filt_map_data)
    } else {
      map <- map |>
        addTrackStopMarkers(filt_map_data)
    }
    
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
        class = "overlay",
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
