

######################################################################################################
######################################################################################################
# Stop segmentation code. 
# NO CHANGES NECESSARY IN THIS CODE.
######################################################################################################

version <- '014'

######################################################################################################

check_data_frame <- function(df) {
  required_columns <- c("animal_id", "timestamp", "latitude", "longitude", "lc", "species")
  
  # Check for presence of all required columns
  if (!all(required_columns %in% names(df))) {
    missing_cols <- setdiff(required_columns, names(df))
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check types
  if (!is.character(df$animal_id)) stop("animal_id must be of type character.")
  if (!inherits(df$timestamp, "POSIXct")) stop("timestamp must be of class POSIXct.")
  if (!is.numeric(df$latitude)) stop("latitude must be numeric.")
  if (!is.numeric(df$longitude)) stop("longitude must be numeric.")
  if (!is.character(df$lc)) stop("lc must be of type character.")
  if (!is.character(df$species)) stop("species must be of type character.")
  
  # Check value ranges
  if (any(df$latitude < -90 | df$latitude > 90, na.rm = TRUE)) {
    stop("latitude values must be between -90 and 90.")
  }
  if (any(df$longitude < -180 | df$longitude > 180, na.rm = TRUE)) {
    stop("longitude values must be between -180 and 180.")
  }

  # Check 'lc' values
  valid_lc <- c("3", "2", "1", "0", "A", "B", "Z", "G")
  if (!all(df$lc %in% valid_lc)) {
    invalid_lc <- unique(df$lc[!df$lc %in% valid_lc])
    stop(paste("Invalid lc values found:", paste(invalid_lc, collapse = ", ")))
  }

  message("\nData frame passed all checks.")
  return(TRUE)
}

check_data_frame(data)

# ==========================================================================================================
# ==========================================================================================================

sanitize_studyName <- function(studyName) {
  # Invalid Windows filename characters...
  invalid_chars <- "[<>:\"/\\\\|?*]"
  # Replace invalid characters with an underscore...
  cleaned_studyName <- gsub(invalid_chars, "_", studyName)
  return(cleaned_studyName)
}
cleaned_studyName <- sanitize_studyName(studyName)

resultsPath <- paste0(resultsDir, cleaned_studyName, "/")

# Check if the directory exists...
if (!dir.exists(resultsPath)) {
  dir.create(resultsPath, recursive = TRUE, showWarnings = FALSE)
  message("\nDirectory created: ", resultsPath, "\n")
} else {
  message("\nDirectory already exists: ", resultsPath, "\n")
}


# ==========================================================================================================
# ==========================================================================================================
# Function to find valid stopover periods using recursion and an optimized iterative approach....

# A stopover maximum duration threshold needs to be defined, but so far, its utility is not apparent....
max_days_threshold <- 365000  # Here set to 1000 years, so it has no influence on the results.


find_periods_recursive <- function(df, start_idx, min_hours = minimum_hours_threshold, max_days = max_days_threshold) {
  if(length(start_idx) == 0) start_idx <- nrow(df)
  if (start_idx >= nrow(df)) {
    return(NULL)
  }
  
  results <- list()
  current_start <- start_idx
  
  while(current_start < nrow(df)) {
    # Initialize variables for current period....
    current_idx <- current_start
    valid_period <- FALSE
    
    # Iterative replacement for check_next_point....
    while(current_idx < nrow(df)) {
      # Pre-calculate coordinates matrices for efficiency....
      prev_coords <- matrix(c(df$longitude[current_start:current_idx],
                            df$latitude[current_start:current_idx]), ncol = 2)
      next_coords <- matrix(c(df$longitude[current_idx + 1],
                            df$latitude[current_idx + 1]), ncol = 2)
      
      # Calculate distances....
      distances <- distHaversine(prev_coords, next_coords)
      
      # Check if all distances are within threshold....
      if (all(distances < proximity_distance_meters)) {
        time_diff <- as.numeric(difftime(df$timestamp[current_idx + 1],
                                       df$timestamp[current_start],
                                       units = "hours"))
        
        if (time_diff <= (max_days * 24)) {
          current_idx <- current_idx + 1
          next
        }
      }
      break
    }
    
    # Check if period is valid....
    if (current_idx > current_start) {
      time_diff <- as.numeric(difftime(df$timestamp[current_idx],
                                     df$timestamp[current_start],
                                     units = "hours"))
      
      if (time_diff >= min_hours) {
        results[[length(results) + 1]] <- data.frame(
          animal_id = df$animal_id[current_start],
          start_time = df$timestamp[current_start],
          end_time = df$timestamp[current_idx],
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Move to next potential period....
    current_start <- current_idx + 1
    
    # Add safety check to prevent infinite loops....
    if (current_start >= nrow(df) || current_idx >= nrow(df)) {
      break
    }
  }
  
  # Combine all results....
  if (length(results) > 0) {
    return(do.call(rbind, results))
  } else {
    return(NULL)
  }
}

### ==========================================================================================================
### ==========================================================================================================

  # Process each animal.... this is where the work gets done.....
  results <- data %>%
    group_by(animal_id) %>%
    group_split() %>%
    lapply(function(animal_data) {
      find_periods_recursive(animal_data, 1)
    }
	) %>%
    bind_rows()

  # Stop the script if zero stops were detected.....
  FAIL <- FALSE
  if (nrow(results) == 0) {
    FAIL <- TRUE
    stop("\n\n>>>>EXITING SCRIPT: No stops were detected, try changing the Dist or Dur threshold, or both.", 
	     call. = FALSE)
  } 


### ==========================================================================================================
### ==========================================================================================================

# create a unique stop_ID variable....
results$stop_id <- paste0(format(results$start_time, "%Y%m%d%H%M%S"),'_',
                          format(results$end_time,   "%Y%m%d%H%M%S"),'_',results$animal_id)

n_raw_locations <- nrow(data)  # capture for a printed summary later....

# merge stops with input raw location records....
data2 <- 
  data %>%
  left_join(results, join_by(animal_id, timestamp >= start_time,timestamp <= end_time))

# derive some variables....
data2$duration <- difftime(data2$end_time, data2$start_time, units = "secs")
data2$stop_hours <- as.numeric(data2$duration/3600)
data2$stop_days  <- as.numeric(data2$duration/(3600*24))

data2 <- data2  %>%
  dplyr::select(-duration)

# isolote and annotate raw locations that were not stopped....
transit_locs <- data2 %>%
  subset(is.na(start_time)) %>%
  mutate(stopover = 0) %>%
  mutate(gis_lat = latitude, gis_lon = longitude) %>%
  mutate(original_lat = latitude, 
         original_lon = longitude)

# isolate and annotate the raw locations that were stopped
tier1 <- data2 %>%
  subset(!is.na(start_time)) %>%
  mutate(original_lat = latitude, 
         original_lon = longitude,
		 stopover = 2)
   
# assign a Location Class weighting factor for calculating the
# single "weighted-average" location of the initial individual stopovers... 
tier1 <- tier1 %>%
               mutate(w = 
			   ifelse(
			   lc=='G' , 5000, 
			   ifelse(
			   lc=='4' , 5000, 
			   ifelse(
			   lc=='3' , 1000, 
			   ifelse(
			   lc=='2' ,  500, 
			   ifelse(
			   lc=='1' ,  300, 
			   ifelse(
			   lc=='0' ,   50,
			   ifelse(
			   lc=='A' ,  100,
			   ifelse(
			   lc=='B' ,   10,
			   ifelse(
			   lc=='Z' ,    5,
			   1)
			   )))))))))


#############################################################################
# calcuate the weighted average stopover location, 
# taking into account longitude is circular and can create bad averages when
# a group of stopped locations span across 0 or 180 longitude...	
#############################################################################	

# first longitude (circular).... 
calculate_weighted_longitude <- function(data) {
  # Detect if longitudes cross 180 or 0....
  lon_range <- range(data$longitude, na.rm = TRUE)
  
  if ((lon_range[2] - lon_range[1]) > 180) {
    # If longitude range is > 180, assume crossing the 180 dateline...
    data <- data %>%
      mutate(elon = ifelse(longitude < 0, longitude + 360, longitude))
  } else {
    # Otherwise, use the standard [-180, 180] range for Greenwich crossing
    data <- data %>%
      mutate(elon = ifelse(longitude > 180, longitude - 360, longitude))
  }

  # Compute weighted circular mean
  data <- data %>%
    group_by(stop_id) %>%
    mutate(elon_rad = elon * pi / 180,
           weighted_sin = sum(w * sin(elon_rad)) / sum(w),
           weighted_cos = sum(w * cos(elon_rad)) / sum(w),
           w_circ_mean_rad = atan2(weighted_sin, weighted_cos),
           elon_weighted_mean = w_circ_mean_rad * 180 / pi)

  # Select representative stopover per stop_id
  data <- data %>%
    arrange(stop_id, timestamp) %>%
    filter(row_number() == 1) %>%
    mutate(stopover_lon = ifelse(elon_weighted_mean > 180, 
                                 elon_weighted_mean - 360, 
                                 elon_weighted_mean)) %>%
    select(stop_id, stopover_lon)

  return(data)
}

tier2lon <- calculate_weighted_longitude(tier1)

# Second, latitude.....
tier2lat <- tier1 %>%
  group_by(stop_id) %>%
  summarise(stopover_lat = weighted.mean(latitude,  w), 
				n_locs = n())

# Combine the lat,lon derivations....
tier2 <- 
  tier2lon %>%
  left_join(tier2lat, join_by(stop_id)) %>%
  select(stop_id,stopover_lat,stopover_lon,n_locs)
 	
# merge the derived stopover location into the location records...
tier3 <- tier2 %>%
  left_join(tier1, join_by(stop_id)) 

# extract just one record per stopover...
tier4 <- tier3 %>%
  arrange(stop_id, timestamp) %>%
  group_by(stop_id) %>%
  filter(row_number() == 1) %>%
  mutate(stopover = 1, latitude = NA, longitude = NA, lc = NA,
         original_lat = NA, original_lon = NA)

n_initial_stops <- nrow(tier4)

# prep the locations associated with the stopovers...
tier1 <- tier1 %>%
  mutate(gis_lat = original_lat, 
         gis_lon = original_lon) %>%
  dplyr::select(-latitude, -longitude)

# combine with all of the data records: 
# transit_locs = non-stopped locs, 
# tier1 = stopped raw locs, and 
# tier4 = stopover derived locs...
tier5 <- bind_rows(transit_locs,tier1,tier4) %>%
  arrange(animal_id, timestamp) %>%
  dplyr::select(-w, -latitude, -longitude) %>%
  mutate(gis_elon = 
			   ifelse(
			   gis_lon < 0 & Dateline, gis_lon + 360, 
			   gis_lon
			   ))

# ==============================================================================================	
# Stage output file of all stops....
# ==============================================================================================	
					  
stopovers4output <- tier5 %>%
    subset(stopover == 1) %>%
    rename(latitude=stopover_lat,longitude=stopover_lon) %>%
	dplyr::mutate(locType = 'Stopover') %>%
	dplyr::select(animal_id,species,start_time,end_time,stop_hours,latitude,longitude,
	       locType,n_locs,stop_id) %>%
	na.omit()

# ==============================================================================================	
# Construct an output CSV file name, including path, and write the file to disk....
# ==============================================================================================	

resultsFullName <- paste0(resultsPath,
					"stopovers_v",
					version,
					"_proxMeters_",proximity_distance_meters,
					"_minHours_",minimum_hours_threshold,
					"_",studyName,
					".csv")


# write stopover start and end times to CSV format in user-defined directory....
write.csv(stopovers4output, 
		  file = resultsFullName, 
		  row.names = FALSE, na = " ")


# =======================================================================================
# =======================================================================================
#########################################################################################
#########################################################################################
# Now run the stopover locations through the same proximity analysis to identify 'MetaStops'
#########################################################################################
#########################################################################################

# First, extract just the weighted-mean stopover locations, cross-walk key variables....
# This set using start_time...
  meta0a <- tier5 %>%
    subset(stopover == 1) %>%
    rename(latitude=stopover_lat,longitude=stopover_lon) %>%
	mutate(timestamp=start_time) %>%
	dplyr::select(animal_id,timestamp,latitude,longitude,
	       species,n_locs) %>%
	na.omit()
# Second, extract just the weighted-mean stopover locations, cross-walk key variables....
# This set using end_time...
  meta0b <- tier5 %>%
    subset(stopover == 1) %>%
    rename(latitude=stopover_lat,longitude=stopover_lon) %>%
	mutate(timestamp=end_time) %>%
	dplyr::select(animal_id,timestamp,latitude,longitude,
	       species,n_locs) %>%
	na.omit()

# Combined the 2 sets of stops (one with timestamp=start_time and the other with timestamp=end_time)
  meta0 <- rbind(meta0a, meta0b)
    
# Sort data by animal_id and timestamp.... 
# need more than one stop to consider existence of a metastop....
  meta <- meta0 %>%
    arrange(animal_id, timestamp) %>%
	group_by(animal_id) %>%
	filter(n() >= 2)
  
nrow(meta0)-nrow(meta)
  
# ==============================================================================================	
  # Process each animal.... this is where metastops are identified.....
  resultsMeta <- meta %>%
    group_by(animal_id) %>%
    group_split() %>%
    lapply(function(animal_data) {
      find_periods_recursive(animal_data, 1)
    }
	) %>%
    bind_rows()
# ==============================================================================================	

nrow(resultsMeta)
n_meta_stops <- nrow(resultsMeta)

# create a unique metastop_ID variable....
resultsMeta$metastop_id <- paste0(format(resultsMeta$start_time, "%Y%m%d%H%M%S"),'_',
                          format(resultsMeta$end_time, "%Y%m%d%H%M%S"),'_',resultsMeta$animal_id)

# =======================================================================================
# =======================================================================================
# =======================================================================================
#########################################################################################
#########################################################################################
# Stopover segmentation has finished. 
#########################################################################################
#########################################################################################
# =======================================================================================
# =======================================================================================
# =======================================================================================

# merge metastops with input stopover records....
meta2 <- 
  meta %>%
  left_join(resultsMeta, join_by(animal_id, timestamp >= start_time,timestamp <= end_time)) 
  
meta2$metaduration <- difftime(meta2$end_time, meta2$start_time, units = "secs")
meta2$metastop_hours <- as.numeric(meta2$metaduration/3600)
meta2$metastop_days  <- as.numeric(meta2$metaduration/(3600*24))

meta2 <- meta2  %>%
  dplyr::select(-metaduration)
 
# isolate the detected metastops.... 
tier1meta <- meta2 %>%
  subset(!is.na(start_time)) 
 			   
# DO NOT average the stopover locations associated with each metastop.
# INSTEAD, USE THE STOPOVER WITH THE LARGEST NUMBER OF RAW LOCATIONS
tier2meta <- tier1meta %>%
  group_by(metastop_id) %>%
  mutate(n_meta = n()/2) %>%
  arrange(metastop_id, desc(n_locs)) %>%
  filter(row_number() == 1) %>%
  mutate(gis_lat = latitude, gis_lon = longitude) %>%
  mutate(meta_lat = latitude, meta_lon = longitude) %>%
  dplyr::select(metastop_id,meta_lat,meta_lon,n_meta,gis_lat,gis_lon)
 
# merge the picked metastop location with the meta stop data....
tier3meta <- tier1meta %>%
  left_join(tier2meta, join_by(metastop_id)) 
  
# extract 1 record per metastop....
tier4meta <- tier3meta %>%
  arrange(metastop_id, timestamp) %>%
  group_by(metastop_id) %>%
  filter(row_number() == 1) %>%
  mutate(metastop = 10) %>%
  rename(metastart_time=start_time,metaend_time=end_time) %>%
  dplyr::select(animal_id,metastop,n_meta,metastart_time,metaend_time,
        metastop_id,metastop_hours,metastop_days,
		meta_lat,meta_lon)

# merge the metastops into the fullmonty data set of just single stops....
tier5$metastop <- 0
ready <- 
  tier5 %>%
  left_join(tier4meta, join_by(animal_id, timestamp >= metastart_time,timestamp <= metaend_time)) %>%
  mutate(metastop.x = replace_na(metastop.x,0), metastop.y = replace_na(metastop.y,0)) %>%
  mutate(metastop = metastop.x + metastop.y) %>%
  dplyr::select(-metastop.x, -metastop.y)
  
# isolate just the metastop records.... coerce consistent variable names for the metastops....
justMeta <- ready %>%
  subset(metastop == 10) %>%
  arrange(animal_id,metastart_time) %>%
  group_by(metastop_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(stopover = 3, stop_id = metastop_id, gis_lat = meta_lat, gis_lon = meta_lon,
         stop_hours = metastop_hours, stop_days = metastop_days,
		 n_locs = n_meta, timestamp = metastart_time,
		 start_time = metastart_time, end_time = metaend_time) %>%
  dplyr::select(-n_meta,-metastart_time,-metaend_time,-metastop_id,-metastop_hours,-metastop_days,
         -meta_lat,-meta_lon)
	
# drop the metastop variables....
readyX <- ready %>%
  dplyr::select(-n_meta,-metastart_time,-metaend_time,-metastop_id,-metastop_hours,-metastop_days,
         -meta_lat,-meta_lon)

# add the summarized metastop records to the fullmonty....
ready2 <- rbind(readyX, justMeta)

# sort...
# classify stopover location type...
# calc east longitude value...
ready3 <- ready2 %>%
  arrange(animal_id,timestamp) %>%
  mutate(stopover2 = metastop + stopover) %>%
  mutate(gis_elon = 
			   ifelse(
			   gis_lon < 0 & Dateline, gis_lon + 360, 
			   gis_lon
			   )) 
			   	   
ready4 <- ready3 %>%
  subset(stopover2 != 11) %>%
  dplyr::select(-stop_hours,-stopover,-metastop) %>%
  rename(stopover = stopover2) %>%
  mutate(original_lat = ifelse(stopover == 13, NA, original_lat)) %>%
  mutate(original_lon = ifelse(stopover == 13, NA, original_lon)) %>%
  mutate(lc = ifelse(stopover == 13, NA, lc)) %>%
  dplyr::select(animal_id,timestamp,start_time,end_time,
         stop_id,stop_days,stopover,gis_lat,gis_lon,gis_elon,
		 n_locs,lc,species) %>%
  arrange(animal_id,timestamp,desc(stopover))
  
# =======================================================================================
# =======================================================================================
# Prepare OUTPUT CSV FILES....
# =======================================================================================
# =======================================================================================

  ready5 <- ready4 %>%
    mutate(
      locType = if_else(stopover == 13, "MetaStop",
                if_else(stopover == 12, "Stopped",
                if_else(stopover == 10, "MovementDuringStop",
                if_else(stopover ==  2, "Movement",
                if_else(stopover ==  0, "Movement", "Unclassified"))))),
      latitude = gis_lat,
      longitude = gis_lon,
      n_stops = n_locs,
	  meta_stop_id = stop_id
	  )
	  
   metaStopsOutput <- ready5 %>%
     subset(stopover == 13) %>%
	 select(animal_id,species,start_time,end_time,stop_days,
	        latitude,longitude,locType,n_stops,meta_stop_id)
	 
   locationData <- ready5 %>%
     subset(stopover != 13) %>%
	 select(animal_id,species,timestamp,latitude,longitude,lc,
	        locType,start_time,end_time,stop_id)
	 
# Merge MetaStop information into full monty location output file....
   temp1 <- metaStopsOutput %>%
     mutate(metastart_time = start_time,
         metaend_time  = end_time) %>%
     select(animal_id, n_stops, metastart_time, metaend_time, meta_stop_id)
  
   newLocData <- 
     locationData %>%
     left_join(temp1, join_by(animal_id, timestamp >= metastart_time,timestamp <= metaend_time)) %>%
	 select(animal_id, species, timestamp, latitude, longitude, lc, locType, stop_id, n_stops, meta_stop_id)


# ==============================================================================================	
# Construct output CSV file names, including path, and write the files to disk....
# ==============================================================================================	

resultsFullName <- paste0(resultsPath,
					"metaStops_v",
					version,
					"_proxMeters_",proximity_distance_meters,
					"_minHours_",minimum_hours_threshold,
					"_",studyName,
					".csv")
write.csv(metaStopsOutput, 
		  file = resultsFullName, 
		  row.names = FALSE, na = " ")


resultsFullName <- paste0(resultsPath,
					"locationsAnnotated_v",
					version,
					"_proxMeters_",proximity_distance_meters,
					"_minHours_",minimum_hours_threshold,
					"_",studyName,
					".csv")
write.csv(newLocData, 
		  file = resultsFullName, 
		  row.names = FALSE, na = " ")

# =======================================================================================
# =======================================================================================

###################################################################################################
# Function: summary info for user to be printed to the console near the end of the sript...

info4user <- function() {
	message("=======================================================================")
	message("Maximum Stopover Proximity Distance: ",proximity_distance_meters," (m)")
	message("Minimum Stopover Duration: ", minimum_hours_threshold,"(hrs)")
	message(" ")
	message("Number of input locations processed:\t", n_raw_locations)
	message("Number of intial stopovers identified:\t", n_initial_stops)
	message("Number of meta stopovers identified:\t", n_meta_stops)
	message("  ")
	message("Stopover periods written to: ")
	message("\t",resultsFullName)
	message("=======================================================================")
Sys.sleep(1)
}
#info4user()
###################################################################################################


###################################################################################################
###################################################################################################
# Illustrate the results dynamically in a leaflet/shinny browser....
###################################################################################################
###################################################################################################

###################################################################################################
# Function: Data preparation....

prepare_data <- function() {
  # Check if required objects exist...
  if (!exists("ready4")) stop("Error: 'ready4' dataset is missing.")
  if (!exists("subsetAnimals")) subsetAnimals <- FALSE
  if (!exists("listAnimals")) listAnimals <- NULL
    
  METAstopovers_v10 <- ready4
  message("Number of location records before subsetting: ", nrow(ready4))
  
  # Subsetting based on animal IDs if subsetAnimals is TRUE...
  if (subsetAnimals) {
    if (is.null(listAnimals) || length(listAnimals) == 0) {
      stop("Error: 'listAnimals' is missing or empty while 'subsetAnimals' is TRUE.")
    }
    METAstopovers_v10 <- ready4 %>%
      filter(animal_id %in% listAnimals)
    
    message("=======================================================================")
    message("Subsetting to include these ", length(listAnimals), " animal_ids only:")
    print(listAnimals)
    message("=======================================================================")
  } else {
    message("=======================================================================")
    message("No subsetting requested, so including all animal_ids....")
    message("=======================================================================")
  }
  
  message("Number of locations that will be processed: ", nrow(METAstopovers_v10))
  
  # Validate required columns exist in the dataset...
  required_cols <- c("animal_id", "species", "stopover", "gis_lat", "gis_elon", "n_locs", "timestamp")
  missing_cols <- setdiff(required_cols, colnames(METAstopovers_v10))
  if (length(missing_cols) > 0) {
    stop("Error: Missing required columns in 'METAstopovers_v10': ", paste(missing_cols, collapse = ", "))
  }
  
  # Prepare the Leaflet data...
  dataLeaflet <- METAstopovers_v10 %>%
    mutate(
      myRadius =if_else(stopover == 13, 10,
                if_else(stopover == 12, 3,
                if_else(stopover == 10, 3,
                if_else(stopover == 2, 3,
                if_else(stopover == 0, 3, 99))))),
      locType = if_else(stopover == 13, "MetaStop",
                if_else(stopover == 12, "Stopped",
                if_else(stopover == 10, "MovementDuringStop",
                if_else(stopover ==  2, "Movement",
                if_else(stopover ==  0, "Movement", "Unclassifed"))))),
      latitude = gis_lat,
      longitude = gis_elon,
      n_stops = n_locs,
      myRadius = if_else(stopover == 13, ((n_stops / 10 * myRadius) + myRadius), myRadius)
    ) %>%
    dplyr::select(animal_id, timestamp, latitude, longitude, lc, stop_days, locType, myRadius, stop_id, n_stops, species) %>%
    ungroup()
  
  dataLeaflet$animal_id <- as.factor(dataLeaflet$animal_id)
  
  # Validate and filter coordinates...
  dataLeaflet <- dataLeaflet %>%
    filter(latitude >= -90 & latitude <= 90) %>%
    filter(longitude >= -180 & longitude <= 360)
  
  # Sort by animal_id and timestamp...
  dataLeaflet <- dataLeaflet %>%
    arrange(animal_id, timestamp)

  message("Data preparation complete. Ready for visualization.")
  
# Define color palettes....
  pal <- colorFactor(c("red", "blue", "cyan", "yellow"),
       domain = c("MetaStop", "Stopped", "MovementDuringStop", "Movement"))

  lc_colors <- c(
   "MetaStop" = "red",
   "Stopped" = "yellow",
   "MovementDuringStop" = "cyan",
   "Movement" = "blue"
  )
  
  ts_min <- as.POSIXct(min(dataLeaflet$timestamp))
  ts_max <- as.POSIXct(min(dataLeaflet$timestamp))
  
  # Return the prepared data and palettes...
  return(list(dataLeaflet = dataLeaflet, palette = pal, lc_colors = lc_colors,
              ts_min = ts_min, ts_max = ts_max))

}

######################################################################################################
# Function: Initialize base map....

create_base_map <- function() {
  leaflet(options = leafletOptions(maxZoom = 22)) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addLayersControl(
      baseGroups = c( "Satellite", "OpenStreetMap"),
      overlayGroups = character(0),  # Start with empty overlay groups...
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
	addDrawToolbar(
    editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
    ) %>% 
    addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers") %>%
	addScaleBar(position = "bottomleft",  # Position of the scale bar...
              options = scaleBarOptions(metric = TRUE,  # Show metric units (meters/kilometers)...
                                         imperial = FALSE,  # Do not show imperial units (feet/miles)...
                                         maxWidth = 400))  # Max width of the scale bar...
}

###################################################################################################
# Function: Add tracking data to map....

add_tracking_data <- function(map, data, time_range) {
  # Filter data based on time range....
  filtered_data <- data %>%
    filter(timestamp >= time_range[1] & timestamp <= time_range[2])
  
  # Create unique colors for each animal, NOT USED....
  animals <- unique(filtered_data$animal_id)
  animal_colors <- colorFactor(
    palette = "Set3",
    domain = animals
  )
  
  # Remove existing overlay groups and add new ones....
  map %>%
    clearGroup(group = animals) %>%
	#addRasterImage(
    # geotiff,
	# group="Sentinel",
    #opacity = 0.5
    #)  %>%
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      overlayGroups = animals,
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    {
      m <- .
      for(animal in animals) {
        animal_data <- filtered_data[filtered_data$animal_id == animal,]
        
        # Add track line....
        m <- m %>%
          addPolylines(
            data = animal_data,
            lat = ~latitude,
            lng = ~longitude,
            color = "white",
			weight = 1,
            opacity = 0.3,
            group = animal,
            options = pathOptions(zIndexOffset = 100)
          )
		  
        # Add points....
        m <- m %>%
          addCircleMarkers(
            data = animal_data,
            lng = ~longitude,
            lat = ~latitude,
            color = ~pal(locType),
            radius = ~myRadius,
            fillOpacity = 0.8,
            stroke = FALSE,
            popup = ~paste(
              "Animal:", animal_id, "<br>",
			  "Species:", species, "<br>",
              "Time:", timestamp, "<br>",
              "Location Type:", locType, "<br>",
			  "Location Class:", lc, "<br>",
              "Stop Days:", stop_days, "<br>",
			  "N stops:", n_stops, "<br>",
			  "Stop ID:", stop_id, "<br>",
              "Coordinates:", round(latitude, 4), ",", round(longitude, 4)
            ),
            group = animal,
            options = pathOptions(zIndexOffset = 200)
          )
      }
      m
   }
}

###################################################################################################
# Need to calcuate the frame-rate (step) for the visualization time slider, based on user input....

if (exists("frameRate") && is.numeric(frameRate) && length(frameRate) == 1) {
  definedSeed <- frameRate * 3600
} else {
  definedSeed <- 86400  # default to 1-day frame rate....
}

###################################################################################################
# Function: Define UI....

build_ui <- function() {

ui <- fluidPage(
  tags$style(type = "text/css", "
    .container-fluid {
      padding: 0;
      margin: 0 20px;  /* Add left and right margins */
      max-width: calc(100% - 40px);  /* Adjust max-width to account for margins */
    }
    .row {
      margin: 0;
      width: 100%;
    }
    .controls-container {
      padding: 10px 15px;
      background-color: #f8f9fa;
      border-bottom: 1px solid #dee2e6;
    }
    .legend-container {
      display: flex;
      flex-direction: row;
      align-items: center;
      justify-content: center;
      gap: 40px;
      margin-top: 15px;
      width: 100%;
      padding: 5px 0;
      background-color: #ffffff;
    }
    .legend-item {
      display: flex;
      align-items: center;
      white-space: nowrap;
      padding: 3px 8px;
      border-radius: 4px;
      background-color: rgba(255, 255, 255, 0.9);
    }
    .time-slider {
      margin-bottom: 5px;
      padding-bottom: 5px;
    }
  "),
#titlePanel("Animal Tracking Map"),
  # Controls section at the top
  div(class = "controls-container",
    # Time slider
    div(class = "time-slider",
      sliderInput("timeRange",
                  paste("Maximum Stopover Proximity Distance:",proximity_distance_meters," (m) -- Minimum Stopover Duration:", 
				  minimum_hours_threshold,"(hrs) -- Study Name:",studyName),
                  min = as.POSIXct(min(dataLeaflet$timestamp)),
                  max = as.POSIXct(max(dataLeaflet$timestamp)),
                  value = c(
                    as.POSIXct(min(dataLeaflet$timestamp)),
                    as.POSIXct(max(dataLeaflet$timestamp))
                  ),
                  width = "100%",  # Make slider use full width of sidebar
                  timeFormat = "%Y-%m-%d",
				  step = definedSeed,  # user selected....
                  #step = 86400, # one day
				  #step = 7200,   # two hours
				  animate = TRUE), # Step size of 1 day in seconds
    
    # Legend
    div(class = "legend-container",
      div(class = "legend-item",
        div(style = sprintf("width: 12px; height: 12px; background-color: %s; margin-right: 5px;", lc_colors["MetaStop"])),
        span("Metastop location, derived", style = "font-size: 13px; font-weight: 500;")
      ),
      div(class = "legend-item",
        div(style = sprintf("width: 12px; height: 12px; background-color: %s; margin-right: 5px;", lc_colors["Stopped"])),
        span("Stopped", style = "font-size: 13px; font-weight: 500;")
      ),
      div(class = "legend-item",
        div(style = sprintf("width: 12px; height: 12px; background-color: %s; margin-right: 5px;", lc_colors["MovementDuringStop"])),
        span("Movement during stop", style = "font-size: 13px; font-weight: 500;")
      ),
      div(class = "legend-item",
        div(style = sprintf("width: 12px; height: 12px; background-color: %s; margin-right: 5px;", lc_colors["Movement"])),
        span("Movement", style = "font-size: 13px; font-weight: 500;")
      )
    )
    )
  ),
  
  # Map section below controls
  div(
    tags$style(type = "text/css", "
        #map {
          height: calc(100vh - 200px) !important;  /* Viewport height minus space for header/controls */
          width: 100% !important;                  /* Full width of container */
          max-width: 100%;                         /* Prevent overflow */
        }
        .leaflet-container {
          height: 100% !important;
          width: 100% !important;
        }
      "),
      leafletOutput("map", height = "calc(100vh - 130px)")  # Reduced space for controls
  )
)
return(list(ui = ui))
}

###################################################################################################
# Function: Define server....

server <- function(input, output, session) {
  # Create the initial base map...
  output$map <- renderLeaflet({
    create_base_map()
  })
  
  # Update tracking data when time range changes...
  observe({
    req(input$timeRange)
    leafletProxy("map") %>%
      clearGroup(group = unique(dataLeaflet$animal_id)) %>%
      add_tracking_data(dataLeaflet, input$timeRange) 
  })
}

###################################################################################################
# Prepare and print a summary of all animal tracks to the console to assist in optionally
#   defining a subset of animals to visualize....
###################################################################################################

###################################################################################################
# Function: Summarize tracks (distance, duration, sample size)....

summarize_tracking_data <- function(dataWork) {
  # Ensure necessary columns exist
  required_cols <- c("animal_id", "timestamp", "gis_lat", "gis_lon")
  missing_cols <- setdiff(required_cols, colnames(dataWork))
  if (length(missing_cols) > 0) {
    stop("Error: Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  nrow(dataWork)
  # Sort dataWork by animal_id and timestamp
  dataWork <- dataWork %>%
    arrange(animal_id, timestamp) %>%
	group_by(animal_id) %>%
	filter(n() >= 2) %>%
	select(animal_id, timestamp, gis_lat, gis_lon) %>%
	na.omit() %>%
	ungroup()
  nrow(dataWork)
  
  # Function to calculate total distance moved by an animal
   calculate_distance <- function(latitudes, longitudes) {
    # Using geosphere::distVincentySphere to calculate distances
      dist <- distVincentySphere(cbind(longitudes[-length(longitudes)], latitudes[-length(latitudes)]),
                                 cbind(longitudes[-1], latitudes[-1]))
      sum(dist, na.rm = TRUE)  # Sum of all distances
  }
  
  # Summarize dataWork by animal_id....
  summary <- dataWork %>%
    group_by(animal_id) %>%
    summarise(
      start_date = min(timestamp),
      end_date = max(timestamp),
      tot_days = round(as.numeric(difftime(max(timestamp), min(timestamp), units = "days")),0),
      tot_dist_km = (calculate_distance(gis_lat, gis_lon)) / 1000.,
	  n_locs = round(n(),0),
      percent = round((n_locs / nrow(dataWork)) * 100, 2)
    ) %>%
	arrange(desc(n_locs)) %>%
    ungroup()
	
  return(summary)
}

dataSummary <- summarize_tracking_data(ready4)
# Convert start and end timestamps to Date format for simplicity....
dataSummary <- dataSummary %>%
    mutate(start_date = as.Date(start_date), end_date = as.Date(end_date))
	
###################################################################################################
# Print summary info to the console....
info4user()
print(dataSummary, n = Inf)

# Define UI
uiF <- fluidPage(
   titlePanel("Animal Selection"),
   DTOutput("summary_table"),
   fluidRow(
     column(3, actionButton("select_all_btn", "Select All")),
     column(3, actionButton("deselect_all_btn", "Deselect All")),
     column(3, actionButton("submit_btn", "Submit Selection")),
     column(3, actionButton("stop_btn", "Stop Script", class = "btn-danger"))
   ),
   verbatimTextOutput("status_msg")
)

# Define serverF
serverF <- function(input, output, session) {

   output$summary_table <- renderDT({
     datatable(dataSummary,
               selection = 'multiple',
               rownames = FALSE,
               options = list(dom = 'tp', pageLength = 15))
   })

   proxy <- dataTableProxy("summary_table")

   observeEvent(input$select_all_btn, {
     selectRows(proxy, 1:nrow(dataSummary))
   })

   observeEvent(input$deselect_all_btn, {
     selectRows(proxy, NULL)
   })

   observeEvent(input$submit_btn, {
     selected_rows <- input$summary_table_rows_selected
     listAnimals <- dataSummary$animal_id[selected_rows]
     stopApp(listAnimals)
   })

   observeEvent(input$stop_btn, {
     stopApp(NULL)
   })
}

###################################################################################################
# Clean up....
suppressWarnings({
rm(list = c("data2", "justMeta", "mbLoc_df", "mbLoc_dfg", "mbRef_df", "meta", "meta0", "meta0a",
            "meta0b", "meta2", "ready", "ready2", "ready3", "ready5", "readyX", "results", "resultsMeta",
			"temp1", "tier1", "tier1meta", "tier2", "tier2lat", "tier2lon", "tier2meta",
			"tier3", "tier3meta", "tier5", "transit_locs"))
			})
###################################################################################################
# End of main program....
###################################################################################################
