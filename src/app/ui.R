# Assemble the app UI
segmentationUI <- function(ns, min_hours, proximity, start, end, step) {
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
    
    # Styles
    includeCSS("www/styles.css"),
    
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
        leafletOutput(ns("map"), height = "calc(100vh - 50px)")
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
        div(class = "markdown-body", includeHTML("app_info.html"))
      )
    )
  )
}

# Absolute panel containing segmentation algorithm parameters and time slider
seg_panel <- function(ns, proximity, min_hours, start, end, step) {
  absolutePanel(
    class = "slider-panel",
    id = "seg-panel",
    top = 54,
    seg_panel_header(),
    proximity_input(proximity),
    duration_input(min_hours),
    actionButton(ns("recalc"), "Identify stop locations"),
    hr(),
    time_range_slider(start, end, step)
  )
}

# App inputs
time_range_slider <- function(ns, start, end, step) {
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

seg_panel_header <- function() {
  tagList(
    h4("Segmentation Parameters"),
    p("Set the location distance and time used to identify stops."),
    p("For more information, see the App Details tab.")
  )
}

proximity_input <- function(ns, proximity) {
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

duration_input <- function(ns, min_hours) {
  numericInput(
    ns("min_hours"),
    span("Minimum stop duration (hours)", icon("question-circle", id = "dur-info")),
    min = 0,
    value = min_hours
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

seg_info <- function() {
  paste0(
    "For more information on these parameters, see the App Details tab."
  )
}
