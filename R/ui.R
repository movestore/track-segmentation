
# build_ui <- function(proximity, min_hours, name) {
#   
#   ui <- fluidPage(
#     tags$style(type = "text/css", "
#     .container-fluid {
#       padding: 0;
#       margin: 0 20px;  /* Add left and right margins */
#       max-width: calc(100% - 40px);  /* Adjust max-width to account for margins */
#     }
#     .row {
#       margin: 0;
#       width: 100%;
#     }
#     .controls-container {
#       padding: 10px 15px;
#       background-color: #f8f9fa;
#       border-bottom: 1px solid #dee2e6;
#     }
#     .legend-container {
#       display: flex;
#       flex-direction: row;
#       align-items: center;
#       justify-content: center;
#       gap: 40px;
#       margin-top: 15px;
#       width: 100%;
#       padding: 5px 0;
#       background-color: #ffffff;
#     }
#     .legend-item {
#       display: flex;
#       align-items: center;
#       white-space: nowrap;
#       padding: 3px 8px;
#       border-radius: 4px;
#       background-color: rgba(255, 255, 255, 0.9);
#     }
#     .time-slider {
#       margin-bottom: 5px;
#       padding-bottom: 5px;
#     }
#   "),
#     #titlePanel("Animal Tracking Map"),
#     # Controls section at the top
#     div(class = "controls-container",
#         # Time slider
#         div(class = "time-slider",
#             sliderInput("timeRange",
#                         paste("Maximum Stopover Proximity Distance:",proximity," (m) -- Minimum Stopover Duration:",
#                               min_hours,"(hrs) -- Study Name:",name),
#                         min = as.POSIXct(min(dataLeaflet$timestamp)),
#                         max = as.POSIXct(max(dataLeaflet$timestamp)),
#                         value = c(
#                           as.POSIXct(min(dataLeaflet$timestamp)),
#                           as.POSIXct(max(dataLeaflet$timestamp))
#                         ),
#                         width = "100%",  # Make slider use full width of sidebar
#                         timeFormat = "%Y-%m-%d",
#                         step = definedSeed,  # user selected....
#                         #step = 86400, # one day
#                         #step = 7200,   # two hours
#                         animate = TRUE), # Step size of 1 day in seconds
#             
#             # Legend
#             div(class = "legend-container",
#                 div(class = "legend-item",
#                     div(style = sprintf("width: 12px; height: 12px; background-color: %s; margin-right: 5px;", lc_colors["MetaStop"])),
#                     span("Metastop location, derived", style = "font-size: 13px; font-weight: 500;")
#                 ),
#                 div(class = "legend-item",
#                     div(style = sprintf("width: 12px; height: 12px; background-color: %s; margin-right: 5px;", lc_colors["Stopped"])),
#                     span("Stopped", style = "font-size: 13px; font-weight: 500;")
#                 ),
#                 div(class = "legend-item",
#                     div(style = sprintf("width: 12px; height: 12px; background-color: %s; margin-right: 5px;", lc_colors["MovementDuringStop"])),
#                     span("Movement during stop", style = "font-size: 13px; font-weight: 500;")
#                 ),
#                 div(class = "legend-item",
#                     div(style = sprintf("width: 12px; height: 12px; background-color: %s; margin-right: 5px;", lc_colors["Movement"])),
#                     span("Movement", style = "font-size: 13px; font-weight: 500;")
#                 )
#             )
#         )
#     ),
#     
#     # Map section below controls
#     div(
#       tags$style(type = "text/css", "
#         #map {
#           height: calc(100vh - 200px) !important;  /* Viewport height minus space for header/controls */
#           width: 100% !important;                  /* Full width of container */
#           max-width: 100%;                         /* Prevent overflow */
#         }
#         .leaflet-container {
#           height: 100% !important;
#           width: 100% !important;
#         }
#       "),
#       leafletOutput("map", height = "calc(100vh - 130px)")  # Reduced space for controls
#     )
#   )
#   return(list(ui = ui))
# }

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
      sliderInput("min-hours", "Minimum hours threshold", min = 0, max = 24, value = min_hours)
    )
  )
}

ui2 <- function(data, min_hours, proximity, step) {
  tagList(
    time_range_slider(data, step),
    segmentation_panel(proximity = proximity, min_hours = min_hours),
    # div(
    #   tags$style(type = "text/css", "
    #     #map {
    #       height: calc(100vh - 200px) !important;  /* Viewport height minus space for header/controls */
    #       width: 100% !important;                  /* Full width of container */
    #       max-width: 100%;                         /* Prevent overflow */
    #     }
    #     .leaflet-container {
    #       height: 100% !important;
    #       width: 100% !important;
    #     }
    #   "),
    leafletOutput("map", height = "calc(100vh - 130px)")  # Reduced space for controls
    # )
  )
}
