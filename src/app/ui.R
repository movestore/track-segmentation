# Assemble the app UI
segmentationUI <- function(ns, 
                           min_hours = 6, 
                           proximity = 150, 
                           start = time_range_slider_start(), 
                           end = time_range_slider_end(), 
                           step = 86400) {
  tagList(
    shinyjs::useShinyjs(), # For dynamic styling of segmentation action button
    
    # For tooltips when hovering over segmentation parameters
    shinyBS::bsTooltip(
      id = "proximity-info",
      title = proximity_info(),
      placement = "right"
    ),
    shinyBS::bsTooltip(
      id = "dur-info",
      title = duration_info(),
      placement = "right"
    ),
    shinyBS::bsTooltip(
      id = "thin-info",
      title = thin_info(),
      placement = "right"
    ),
    
    # Styles
    includeCSS("src/app/styles.css"),
    
    # Tabset panel containing map, data tables, and about tabs
    tabsetPanel(
      # Segmentation parameter inputs and map of results
      tabPanel(
        "Map",
        seg_panel(
          ns,
          proximity = proximity,
          min_hours = min_hours,
          start = start,
          end = end,
          step = step
        ),
        leafletOutput(ns("map"), height = "calc(100vh - 84px)")
      ),
      
      # Data tables for identified stop and metastop locations
      tabPanel(
        "Results",
        div(
          id = "data-wrapper",
          style = "position:relative; min-height:300px;",
          uiOutput(ns("data_contents")),
          uiOutput(ns("data_overlay")) # Mask empty results before algorithm has run
        )
      ),
      
      # Algorithm details. From HTML fragment created separately by app_info.Rmd
      tabPanel(
        "App Details",
        div(class = "markdown-body", includeHTML("src/app/app_info.html"))
      )
    )
  )
}

# Absolute panel containing segmentation algorithm parameters and time slider
seg_panel <- function(ns, proximity, min_hours, start, end, step) {
  absolutePanel(
    class = "slider-panel",
    id = "seg-panel",
    top = 88,
    seg_panel_header(),
    proximity_input(ns, proximity),
    duration_input(ns, min_hours),
    actionButton(ns("recalc"), "Identify stop locations"),
    actionButton(ns("write"), "Write results"),
    hr(),
    viz_panel_header(),
    time_range_slider(ns, start, end, step),
    thin_input(ns, max_pts = 1000)
  )
}

# App inputs
time_range_slider <- function(ns, 
                              start = time_range_slider_start(), 
                              end = time_range_slider_end(), 
                              step = 86400) {
  sliderInput(
    ns("time_range"),
    "Time range of interest",
    min = start,
    max = end,
    value = c(start, end),
    timezone = "UTC",
    timeFormat = "%Y-%m-%d",
    step = step
  )
}

# Default values for initial time range slider.
# Slider will be updated after data are ingested to reflect the data time range
time_range_slider_start <- function() {
  as.POSIXct("2000-01-01 00:00:00", "UTC")
}

time_range_slider_end <- function() {
  as.POSIXct("2001-01-01 00:00:00", "UTC")
}

seg_panel_header <- function() {
  tagList(
    h4("Segmentation parameters"),
    p("Set the location distance and time used to identify stops."),
    p("For more information, see the App Details tab.")
  )
}

viz_panel_header <- function() {
  tagList(
    h4("Visualization parameters")
  )
}

proximity_input <- function(ns, proximity = 150) {
  numericInput(
    ns("proximity"),
    span(
      "Maximum distance between stopped locations (meters)",
      icon("question-circle", id = "proximity-info")
    ),
    min = 0,
    value = proximity
  )
}

duration_input <- function(ns, min_hours = 6) {
  numericInput(
    ns("min_hours"),
    span("Minimum stop duration (hours)", icon("question-circle", id = "dur-info")),
    min = 0,
    value = min_hours
  )
}

thin_input <- function(ns, max_pts) {
  tagList(
    checkboxInput(
      ns("should_thin"), 
      span("Thin map points", icon("question-circle", id = "thin-info")), 
      FALSE
    ),
    numericInput(ns("n_thin"), "Max points to plot (per individual)", value = max_pts)
  )
}

# App tooltip text
proximity_info <- function() {
  paste0(
    "Set the maximum allowable pairwise distance (in meters) among all ",
    "locations that constitute a stop. ",
    "Locations belonging to a given stop will all be within this distance of ",
    "one another."
  )
}

duration_info <- function() {
  paste0(
    "Set the minimum required duration that a stop must persist. ",
    "The elapsed time between the first and last locations in a given stop ",
    "will be greater than this value."
  )
}

thin_info <- function() {
  paste0(
    "Check this box to display only a subset of tracked locations on the map. ",
    "Set the number of points to display for each animal in the box below. ",
    "Identified metastop locations will always be retained in the output map. ",
    "Use this setting to improve map responsiveness for large datasets."
  )
}

seg_info <- function() {
  paste0(
    "For more information on these parameters, see the App Details tab."
  )
}

toggle_valid_btn <- function(id, invalidated) {
  if (invalidated) {
    shinyjs::removeClass(id, "valid-btn")
    shinyjs::addClass(id, "invalid-btn")
  } else {
    shinyjs::removeClass(id, "invalid-btn")
    shinyjs::addClass(id, "valid-btn")
  }
}
