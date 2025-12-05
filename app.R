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
# options(shiny.fullstacktrace = TRUE)
# This redirects output to tempdir for dev purposes
# TODO: after adapting to MoveApps framework, output location should be 
# handled by .env instead
Sys.setenv("APP_ARTIFACTS_DIR" = tempdir())

# This will ultimately come from previous MoveApp
# data_raw <- readRDS("~/Documents/projects/track-segmentation/data/raw/input2_move2loc_LatLon.rds")
# data_raw <- move2::movebank_download_study(study_id = 438644854, sensor_type_id = "argos-doppler-shift")
# data_raw <- move2::movebank_download_study(study_id = 1718959411, sensor_type = "argos")

# bbox <- sf::st_bbox(data_raw)

data <- move2_to_seg(data_raw)

# check_data_frame(data)

# ------------------------------------------------------------------------------

ui <- fluidPage(
  seg_ui(data = data_raw, min_hours = 6, proximity = 150, step = 86400)
)

server <- function(input, output, session) {
  has_stops <- reactiveVal(FALSE)
  has_metastops <- reactiveVal(FALSE)
  results_zip <- reactiveVal(NULL)
  map_data <- reactiveVal(list(type = "init", data = data))
  bbox <- reactiveVal(sf::st_bbox(data_raw))
  map_trigger <- reactiveVal(0)
  button_invalid <- reactiveVal(TRUE)
  
  bbox_adj <- reactive({
    adj_bbox(bbox(), dateline = input$dateline)
  })
  
  # Ideally need to update the algorithm to simply ingest dl adjusted data...
  # right now the algorithm itself does the adjustment. But presumably
  # we should be able to update up front and still get the same stop/metastop output
  # without the algorithm needing to do this adjustment...
  # dl_adj_data <- reactive({
  #   d <- map_data()$data
  #   req(d)
  # 
  #   d <- 
  # })
  
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
    stops <- tryCatch(
      suppressWarnings(
        id_stops(data, input$min_hours, input$proximity, dateline = input$dateline)
      ),
      error = function(cnd) {
        mutate_empty_stops(data, dateline = input$dateline)
      }
    )
    
    has_stops(TRUE)
    
    list(
      result = stops,
      min_hours = isolate(input$min_hours),
      proximity = isolate(input$proximity)
    )
  })
  
  find_metastops <- eventReactive(find_stops(), {
    stops <- find_stops()
    
    metastops <- tryCatch(
      suppressWarnings(
        id_metastops(stops$result, stops$min_hours, stops$proximity, dateline = input$dateline)
      ),
      error = function(cnd) {
        mutate_empty_metastops(stops$result)
      }
    )
    
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
    create_basemap(isolate(bbox_adj()))
  })
  
  observe({
    input$dateline
    
    bbox <- req(bbox_adj())
    
    leafletProxy("map") |> 
      fitBounds(
        lng1 = bbox[["xmin"]],
        lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]],
        lat2 = bbox[["ymax"]]
      )
  })
  
  # Update tracking data when time range changes...
  observe({
    # Dependency on map render counter ensures that we always re-render
    # map on input$recalc trigger, even if inputs don't change.
    # (We don't want to explicitly trigger this on input$recalc because
    # we need more control over temporal ordering of processing steps)
    map_trigger()
    
    d <- map_data()
    
    filt_map_data <- filt_data()

    req(filt_map_data)
    req(d)
    
    data_status <- d$type
    full_data <- d$data
    
    # Clear existing animals using non-filtered data. Filtered data will no
    # longer contain these animal IDs and they will stick to the map instead of
    # disappearing
    map <- leafletProxy("map") |> 
      clearGroup(group = unique(full_data$animal_id)) 
    
    # Add layer selection and track lines
    map <- map |>
      addTrackLayersControl(filt_map_data) |> 
      addTrackLines(filt_map_data, dateline = input$dateline)
    
    # Initial data do not have all necessary attributes for coloring in the same
    # way as processed data. After the first time stops are calculated, we can
    # render with the stop/metastop styling
    if (data_status == "init") {
      map <- map |>
        addTrackLocationMarkers(filt_map_data, dateline = input$dateline)
    } else {
      map <- map |>
        addTrackStopMarkers(filt_map_data, dateline = input$dateline)
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
