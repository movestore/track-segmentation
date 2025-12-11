library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(shinyBS)

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

# Use sf to identify whether IDL is crossed
crosses_dl <- move2_crosses_dateline(data_raw)
data <- move2_to_seg(data_raw)
bbox <- get_init_bbox(data, crosses_dl)

time_range_start <- as.POSIXct(min(data$timestamp))
time_range_end   <- as.POSIXct(max(data$timestamp))

# check_data_frame(data)

# ------------------------------------------------------------------------------

ui <- fluidPage(
  seg_ui(
    min_hours = 6, 
    proximity = 150, 
    start = time_range_start, 
    end = time_range_end, 
    step = 86400, 
    init_dl = crosses_dl
  )
)

server <- function(input, output, session) {
  has_stops <- reactiveVal(FALSE)
  has_metastops <- reactiveVal(FALSE)
  results_zip <- reactiveVal(NULL)
  map_data <- reactiveVal(list(type = "init", data = data))
  map_trigger <- reactiveVal(0)
  button_invalid <- reactiveVal(TRUE)

  filt_data <- reactive({
    d <- req(map_data()$data)
    
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
      shinyjs::removeClass("recalc", "valid-btn")
      shinyjs::addClass("recalc", class = "invalid-btn")
    } else {
      shinyjs::removeClass("recalc", "invalid-btn")
      shinyjs::addClass("recalc", "valid-btn")
    }
  })
  
  find_stops <- eventReactive(input$recalc, {
    stops <- tryCatch(
      suppressWarnings(
        id_stops(data, input$min_hours, input$proximity, dateline = crosses_dl)
      ),
      error = function(cnd) {
        mutate_empty_stops(data, dateline = crosses_dl)
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
        id_metastops(stops$result, stops$min_hours, stops$proximity, dateline = crosses_dl)
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
    create_basemap(bbox)
  })
  
  # Update map on time range change, change in segmentation data results,
  # or change in map bounds (proxy for dateline change)
  observe({
    # Dependency on map render counter ensures that we always re-render
    # map on input$recalc trigger, even if inputs don't change.
    # (We don't want to explicitly trigger this on input$recalc because
    # we need more control over temporal ordering of processing steps)
    map_trigger()
    
    d <- req(map_data())
    filt_map_data <- req(filt_data())

    filt_map_data <- filt_map_data |> 
      mutate(longitude_adj = get_elon(longitude, dateline = crosses_dl))
    
    # Clear existing animals using non-filtered data. Filtered data will no
    # longer contain these animal IDs and they will stick to the map instead of
    # disappearing
    map <- leafletProxy("map") |> 
      clearGroup(group = unique(d$data$animal_id)) |>
      addTrackLayersControl(filt_map_data) |> # Add layer selection panels
      addTrackLines(filt_map_data) # Add track lines
    
    # Initial data do not have all necessary attributes for coloring in the same
    # way as processed data. After the first time stops are calculated, we can
    # render with the stop/metastop styling
    if (d$type == "init") {
      map <- addTrackLocationMarkers(map, filt_map_data)
    } else {
      map <- addTrackStopMarkers(map, filt_map_data)
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
        "Run the segmentation algorithm to see results here."
      )
    }
  })
  
  output$stop_data <- DT::renderDataTable({
    d <- req(find_stops()$result)
    
    d <- dplyr::filter(
      d, 
      timestamp >= input$timeRange[1], 
      timestamp <= input$timeRange[2]
    )
    
    prettify(prep_stops_output(d))
  })
  
  output$metastop_data <- DT::renderDataTable({
    d <- req(find_metastops())
    
    d <- dplyr::filter(
      d, 
      timestamp >= input$timeRange[1], 
      timestamp <= input$timeRange[2]
    )
    
    prettify(prep_metastops_output(d))
  })
}

shinyApp(ui, server)
