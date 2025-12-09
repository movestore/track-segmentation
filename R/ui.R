
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
    tags$h4("Segmentation Parameters"),
    tags$p("Set the location distance and time used to identify stops."),
    numericInput("proximity", "Maximum distance between stopped locations (meters)", min = 0, value = proximity),
    numericInput("min_hours", "Minimum stop time (hours)", min = 0, value = min_hours),
    actionButton("recalc", "Calculate stop locations"),
    tags$hr(),
    time_range_slider(start, end, step),
    checkboxInput("dateline", "Tracks cross dateline", value = init_dl)
  )
}

seg_ui <- function(min_hours, proximity, start, end, step, init_dl) {
  tagList(
    shinyjs::useShinyjs(),
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
      )
    )
  )
}
