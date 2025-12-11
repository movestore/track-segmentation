
time_range_slider <- function(start, end, step) {
  sliderInput(
    "timeRange",
    "Time range of interest",
    min = start,
    max = end,
    value = c(start, end),
    timezone = "UTC",
    timeFormat = "%Y-%m-%d",
    step = step
  )
}

segmentation_panel <- function(proximity, min_hours, start, end, step, init_dl) {
  absolutePanel(
    class = "slider-panel",
    id = "seg-panel",
    top = 54,
    h4("Segmentation Parameters"),
    p("Set the location distance and time used to identify stops."),
    p("For more information, see the App Details tab."),
    numericInput(
      "proximity", 
      tags$span("Maximum distance between stopped locations (meters)", icon("question-circle", id = "proximity-info")),
      min = 0, 
      value = proximity
    ),
    numericInput(
      "min_hours", 
      tags$span("Minimum stop duration (hours)", icon("question-circle", id = "dur-info")),
      min = 0, 
      value = min_hours
    ),
    actionButton("recalc", "Identify stop locations"),
    tags$hr(),
    time_range_slider(start, end, step)
  )
}

seg_ui <- function(min_hours, proximity, start, end, step, init_dl) {
  tagList(
    shinyjs::useShinyjs(),
    shinyBS::bsTooltip(id = "proximity-info", title = proximity_info(), placement = "right"),
    shinyBS::bsTooltip(id = "dur-info", title = duration_info(), placement = "right"),
    includeCSS("www/styles.css"),
    tabsetPanel(
      tabPanel(
        "Map",
        segmentation_panel(proximity = proximity, min_hours = min_hours, start = start, end = end, step = step, init_dl = init_dl),
        leafletOutput("map", height = "calc(100vh - 50px)")
      ),
      tabPanel(
        "Data",
        div(
          id = "data-wrapper",
          style = "position:relative; min-height:300px;",
          uiOutput("data_contents"),
          uiOutput("data_overlay")
        )
      ),
      tabPanel(
        "App Details",
        div(class = "markdown-body", includeHTML("app_info.html"))
      )
    )
  )
}

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