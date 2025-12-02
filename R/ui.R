
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

overlay_style <- function() {
  "
    position: absolute;
    top: 0; left:0; right:0; bottom:0;
    background: rgba(255,255,255,0.8);
    z-index: 10;
    display: flex;
    justify-content: center;
    align-items: center;
    color: #666;
    font-size: 1.2em;
    text-align: center;
    padding: 30px;
    height: 100%;
  "
}

slider_panel_style <- function() {
  "
    top: 164px;
    left: 80px;
    width:300px;
    position:absolute;
    cursor:inherit;
    z-index: 9999;
    background: rgba(255,255,255,0.9);
    padding: 10px;
    border-radius: 5px;
    border: 1px solid rgba(0, 0, 0, 0.2);
  "
}

segmentation_panel <- function(proximity, min_hours) {
  div(
    absolutePanel(
      style = slider_panel_style(),
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
