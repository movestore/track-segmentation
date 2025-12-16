library(shiny)
library(dplyr)
library(sf)
library(move2)
library(tidyr)
library(leaflet)
library(leaflet.extras)
library(shinyBS)

source("./src/app/ui.R")
source("./src/app/segmentation.R")
source("./src/app/output.R")
source("./src/app/map.R")

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  fluidPage(
    segmentationUI(ns, min_hours = 6, proximity = 150)
  )
}

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data) # For returning input data
  
  # Prep -------------
  
  data <- data |>
    arrange(mt_track_id(data), mt_time(data)) |> # Order by track ID and timestamp
    mt_filter_unique(criterion = "first") # Remove duplicates
  
  data <- data[!st_is_empty(data), ] # Remove empty points
  
  crosses_dl <- move2_crosses_dateline(data)
  
  data <- check_seg_data(move2_to_seg(data))
  
  # Use sf to identify whether IDL is crossed and build appropriate basemap bbox
  bbox <- get_init_bbox(data, crosses_dl)
  
  # ------------------
  
  results_zip <- reactiveVal(NULL)
  is_map_init <- reactiveVal(TRUE)
  cur_map_data <- reactiveVal(data)
  map_trigger <- reactiveVal(0)
  button_invalid <- reactiveVal(TRUE)
  slider_needs_update <- reactiveVal(TRUE)
  
  # Update time range slider endpoints to reflect data time range on app load
  observe({
    if (slider_needs_update()) {
      start <- as.POSIXct(min(data$timestamp))
      end <- as.POSIXct(max(data$timestamp))
      
      updateSliderInput(
        session,
        "time_range",
        min = start,
        max = end,
        value = c(start, end),
        timeFormat = "%Y-%m-%d",
        step = 86400
      )
    }
    
    isolate(slider_needs_update(FALSE))
  })
  
  # Filter map data to the input time range
  filt_map_data <- reactive({
    tr <- req(input$time_range)
    filter(req(cur_map_data()), timestamp >= tr[1], timestamp <= tr[2])
  })
  
  # On algorithm start, launch busy spinner. Separating from algorithm itself
  # ensures spinner starts before processing does.
  observeEvent(input$recalc, priority = 100, {
    shinybusy::show_modal_spinner("radar")
  })
  
  # Track sliders and change action button CSS to indicate when segmentation
  # needs rerun
  observeEvent(list(input$min_hours, input$proximity), {
    button_invalid(TRUE)
  })
  
  observe({
    if (button_invalid()) {
      shinyjs::removeClass(ns("recalc"), "valid-btn")
      shinyjs::addClass(ns("recalc"), class = "invalid-btn")
    } else {
      shinyjs::removeClass(ns("recalc"), "invalid-btn")
      shinyjs::addClass(ns("recalc"), "valid-btn")
    }
  })
  
  # Identify stop locations for the given proximity and duration inputs
  stop_locations <- reactive({
    stops <- tryCatch(
      suppressWarnings(
        find_stop_locations(
          data,
          min_hours = input$min_hours,
          proximity = input$proximity
        )
      ),
      error = function(cnd) {
        mutate_empty_stops(data)
      }
    )

    # Return inputs to ensure metastop processing uses the same inputs as 
    # were used during stop processing. Don't want metastops to be dependent
    # on current input state
    list(
      result = stops,
      min_hours = isolate(input$min_hours),
      proximity = isolate(input$proximity)
    )
  }) |>
    bindCache(input$min_hours, input$proximity) |>
    bindEvent(input$recalc)
  
  # Identify metastop locations based on the output stop locations
  metastop_locations <- eventReactive(stop_locations(), {
    stops <- stop_locations()
    
    metastops <- tryCatch(
      suppressWarnings(
        find_metastop_locations(
          stops$result,
          min_hours = stops$min_hours,
          proximity = stops$proximity
        )
      ),
      error = function(cnd) {
        mutate_empty_metastops(stops$result)
      }
    )

    button_invalid(FALSE) # Action button will now be up to date with map data
    is_map_init(FALSE) # Map no longer needs to show initial unclassified points
    cur_map_data(data_for_leaflet(metastops)) # Update data for mapping
    map_trigger(map_trigger() + 1) # Increment map trigger so we can track when map is re-rendered
    
    metastops
  })
  
  # When stops and metastops have been calculated, write output results zip
  # automatically. Overwrites any existing results, so only the most recent
  # run is stored.
  observeEvent(metastop_locations(), {
    stops <- stop_locations()
    metastops <- metastop_locations()
    
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
  
  # Create the initial base map
  output$map <- renderLeaflet({
    create_basemap(bbox)
  })
  
  # Update map on change in segmentation data results or change in time range
  observe({
    # Dependency on map render counter ensures that we always re-render
    # map on input$recalc trigger, even if inputs don't change.
    # We don't want to explicitly trigger this on input$recalc because
    # we need to wait until all processing is complete before re-rendering
    map_trigger()
    
    d <- req(filt_map_data())
    
    # Handle dateline crossing longitude adjustment on the fly
    d <- d |>
      mutate(longitude_adj = get_elon(longitude, dateline = crosses_dl))
    
    # Clear existing animals using non-filtered data. Filtered data will no
    # longer contain these animal IDs and they will stick to the map instead of
    # disappearing
    map <- leafletProxy(ns("map")) |>
      clearGroup(group = unique(data$animal_id)) |>
      addTrackLayersControl(d) |> # Add layer selection panels
      addTrackLines(d) # Add track lines
    
    # Initial data do not have all necessary attributes for coloring in the same
    # way as processed data. After the first time stops are calculated, we can
    # render with the stop/metastop styling
    if (is_map_init()) {
      map <- addTrackLocationMarkers(map, d)
    } else {
      map <- addTrackStopMarkers(map, d)
    }
    
    shinybusy::remove_modal_spinner()
  })
  
  # Render tabular summary of stop/metastop locations after processing
  output$data_contents <- renderUI({
    tagList(
      h3("Stops"),
      DT::dataTableOutput(ns("stop_data")),
      hr(),
      h3("Metastops"),
      DT::dataTableOutput(ns("metastop_data"))
    )
  })
  
  # If algorithm hasn't been run yet, gray out the results panel
  output$data_overlay <- renderUI({
    if (is_map_init()) {
      div(
        class = "overlay",
        "Run the segmentation algorithm to see results here."
      )
    }
  })
  
  # Tabular outputs
  output$stop_data <- DT::renderDataTable({
    d <- req(stop_locations()$result)
    
    d <- filter(
      d,
      timestamp >= input$time_range[1],
      timestamp <= input$time_range[2]
    )
    
    prettify(prep_stops_output(d))
  })
  
  output$metastop_data <- DT::renderDataTable({
    d <- req(metastop_locations())
    
    d <- filter(
      d,
      timestamp >= input$time_range[1],
      timestamp <= input$time_range[2]
    )
    
    prettify(prep_metastops_output(d))
  })
  
  return(reactive({ current() }))
}
