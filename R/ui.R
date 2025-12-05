
time_range_slider <- function(data, step) {
  sliderInput(
    "timeRange",
    paste(
      "Time range of interest"
      # "Maximum Stopover Proximity Distance:", proximity, 
      # " (m) -- Minimum Stopover Duration:", min_hours,
      # "(hrs) -- ", 
      # "Study Name:", name
    ),
    min = as.POSIXct(min(data$timestamp)),
    max = as.POSIXct(max(data$timestamp)),
    value = c(
      as.POSIXct(min(data$timestamp)),
      as.POSIXct(max(data$timestamp))
    ),
    width = "100%",
    timeFormat = "%Y-%m-%d",
    step = step,
    animate = TRUE
  )
}

segmentation_panel <- function(proximity, min_hours) {
  div(
    absolutePanel(
      class = "slider-panel",
      top = 164, 
      left = 80, 
      width = 300,
      sliderInput("proximity", "Max distance proximity", min = 0, max = 1000, value = proximity),
      sliderInput("min_hours", "Minimum hours threshold", min = 0, max = 24, value = min_hours),
      checkboxInput("dateline", "Tracks cross dateline", value = FALSE),
      actionButton("recalc", "Recalculate stops")
    )
  )
}

seg_ui <- function(data, min_hours, proximity, step) {
  tagList(
    shinyjs::useShinyjs(),
    includeCSS("www/styles.css"),
    tabsetPanel(
      tabPanel(
        "Map",
        time_range_slider(data, step),    
        segmentation_panel(proximity = proximity, min_hours = min_hours),
        leafletOutput("map", height = "calc(100vh - 175px)")
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
