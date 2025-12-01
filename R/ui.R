
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
      style = "
          z-index: 9999;
          background: rgba(255,255,255,0.8);
          padding: 5px;
          border-radius: 5px;
        ",
      top = 125, 
      left = 75, 
      width = 300,
      sliderInput("proximity", "Max distance proximity", min = 0, max = 1000, value = proximity),
      sliderInput("min_hours", "Minimum hours threshold", min = 0, max = 24, value = min_hours),
      actionButton("recalc", "Recalculate stops")
    )
  )
}

seg_ui <- function(data, min_hours, proximity, step) {
  tagList(
    tabsetPanel(
      tabPanel(
        "Map",
        time_range_slider(data, step),    
        segmentation_panel(proximity = proximity, min_hours = min_hours),
        leafletOutput("map", height = "calc(100vh - 130px)")
      ),
      tabPanel(
        "Data",
        h3("Stops"),
        DT::dataTableOutput("stop_data"),
        hr(),
        h3("Metastops"),
        DT::dataTableOutput("metastop_data")
      )
    )
  )
}
