m1 <- readRDS("../../data/raw/input1_move2loc_LatLon.rds")
m2 <- readRDS("../../data/raw/data_godwit_subset.rds") # For testing dateline behavior

d1 <- move2_to_seg(m1)
d2 <- move2_to_seg(m2)

stops <- find_stop_locations(d1, min_hours = 6, proximity = 150)
metastops <- find_metastop_locations(stops, min_hours = 6, proximity = 150)

test_that("Can initialize map bbox", {
  expect_equal(
    get_init_bbox(d2, FALSE), 
    sf::st_bbox(
      c(xmin = -165.761, ymin = -18.236, xmax = 177.032, ymax = 75.600)
    )
  )
  expect_equal(
    get_init_bbox(d2, TRUE), 
    sf::st_bbox(
      c(xmin = 116.934, ymin = -18.236, xmax = 196.010, ymax = 75.600)
    )
  )
  expect_equal(
    get_init_bbox(d1, FALSE),
    sf::st_bbox(
      c(xmin = 14.92163, ymin = 37.83037, xmax = 14.96063, ymax = 37.87178)
    ),
    tolerance = 0.00001
  )
  # Dateline shouldn't affect inputs that do not cross the dateline
  expect_equal(
    get_init_bbox(d1, FALSE),
    get_init_bbox(d1, TRUE)
  )
})

test_that("Can identify whether dateline is crossed", {
  expect_false(move2_crosses_dateline(m1))
  expect_true(move2_crosses_dateline(m2))
})

test_that("Can initialize basemap", {
  m <- create_basemap(get_init_bbox(d1, FALSE))

  basemap_methods <- unlist(lapply(m$x$calls, function(x) x$method))

  expect_equal(
    basemap_methods,
    c(
      "addTiles", "addProviderTiles", "addDrawToolbar", "addMeasure",
      "addScaleBar", "addLayersControl", "addControl", "createMapPane",
      "createMapPane", "createMapPane"
    )
  )
})

# Some basic proxy tests to ensure that the segmentation results
# are as expected. Obviously this does not ensure that the specific 
# results are correct, but given that these have been independently
# verified against original app results, this will at least tell us whether
# updates change the results we get out of the algorithm
test_that("Correct segmentation results", {
  stop_records <- stops |> filter(!is.na(stopover_lat))
  metastop_records <- metastops |> filter(locType == "Metastop")
  
  expect_equal(n_stops, 142)
  expect_equal(nrow(stops), nrow(d1) + nrow(stop_records))
  
  expect_equal(
    stop_records$stop_id[1], 
    "20210701200400_20210702020900_Goat.8810_1600804509"
  )
  expect_equal(
    stop_records$stop_id[nrow(stop_records)], 
    "20220314160900_20220315060700_Goat.8810_1600804509"
  )
  
  expect_equal(n_metastops, 32)
  expect_equal(nrow(metastops), nrow(d1) + nrow(metastop_records))
  
  expect_equal(
    metastop_records$stop_id[1], 
    "20210701200400_20210702020900_Goat.8810_1600804509"
  )
  expect_equal(
    metastop_records$stop_id[nrow(metastop_records)], 
    "20220314160900_20220315060700_Goat.8810_1600804509"
  )
})

test_that("Expected initial map", {
  testthat::local_edition(3)
  
  map <- create_basemap(get_init_bbox(d1, FALSE))
  
  # Plot subset of data to reduce snapshot file size
  map_data <- d1[1:500,] |> 
    mutate(longitude_adj = get_elon(longitude, dateline = FALSE))
  
  map <- map |> 
    addTrackLayersControl(map_data) |>
    addTrackLines(map_data) |> 
    addTrackLocationMarkers(map_data)
  
  expect_snapshot(map$x)
})

test_that("Expected classified map", {
  testthat::local_edition(3)
  
  map <- create_basemap(get_init_bbox(d1, FALSE))
  
  # Plot subset of data to reduce snapshot file size
  map_data <- data_for_leaflet(metastops)[1:500,] |> 
    mutate(longitude_adj = get_elon(longitude, dateline = FALSE))
  
  map <- map |> 
    addTrackLayersControl(map_data) |>
    addTrackLines(map_data) |> 
    addTrackStopMarkers(map_data)
  
  expect_snapshot(map$x)
})

test_that("Can write results to zip", {
  my_tmp <- paste0(tempdir(), "/")
  withr::local_envvar(list(APP_ARTIFACTS_DIR = my_tmp))
  
  f_out <- write_results(
    stops,
    metastops,
    proximity = 150,
    min_hours = 6
  )
  
  internal <- zip::zip_list(f_out)
  
  expect_true(
    file.exists(
      paste0(my_tmp, "track_segmentation_v014_proxMeters150_minHours6.zip")
    )
  )
  expect_true(grepl("^stopovers_", internal$filename[1]))
  expect_true(grepl("^metaStops_", internal$filename[2]))
  expect_true(grepl("^locationsAnnotated_", internal$filename[3]))
  
  s_out <- readr::read_csv(unz(f_out, internal$filename[1]))
  m_out <- readr::read_csv(unz(f_out, internal$filename[2]))
  l_out <- readr::read_csv(unz(f_out, internal$filename[3]))
  
  expect_equal(nrow(s_out), 142)
  expect_equal(nrow(m_out), 32)
  expect_equal(nrow(l_out), nrow(d1))
  
  expect_equal(
    s_out$stop_id[1], 
    "20210701200400_20210702020900_Goat.8810_1600804509"
  )
  expect_equal(
    s_out$stop_id[nrow(s_out)], 
    "20220314160900_20220315060700_Goat.8810_1600804509"
  )
  
  expect_equal(
    m_out$meta_stop_id[1], 
    "20210701200400_20210702020900_Goat.8810_1600804509"
  )
  expect_equal(
    m_out$meta_stop_id[nrow(m_out)], 
    "20220314160900_20220315060700_Goat.8810_1600804509"
  )
  
  # Metastops not included as these are directly annotated location records
  expect_setequal(
    l_out$locType, 
    c("Movement", "Movement during stop", "Stopped")
  )
})
