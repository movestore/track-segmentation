
# ==========================================================================================================
# ==========================================================================================================
# Download Movebank data....
# ==========================================================================================================

# Define Movebank Login Credentials....
Login<-movebankLogin(username=myUser,password=myPass)

suppressWarnings(searchMovebankStudies(studyName, login=Login))

### Read the associated Movebank reference data into a data frame....
#mbRef_df<-getMovebankReferenceTable(study=studyName,login=Login) %>%
mbRef_df<-getMovebankReferenceTable(study=studyName,login=Login) %>%
   dplyr::select(deployment_id, sensor_type_id, animal_taxon_canonical_name,
                 animal_timestamp_start, animal_timestamp_end, deploy_on_timestamp)
				 
nArgos <- mbRef_df %>% filter(sensor_type_id == 82798) %>% nrow()				 
nGPS   <- mbRef_df %>% filter(sensor_type_id == 653)   %>% nrow()

cat("\n========================================================================================================\n")
			 
### Read any Argos Movebank tracking data into a data frame....
if( nArgos >= 1) {
    mbLoc_dfa<- getMovebankLocationData(study=studyName, 
                  sensorID="Argos Doppler Shift",login=Login)
				  message("Argos data downloaded successfully for ",nArgos," animals.")
				  flush.console()
				  }

### Read any GPS Movebank tracking data into a data frame....
if( nGPS >= 1) {
    mbLoc_dfg<- getMovebankLocationData(study=studyName, 
                  sensorID="GPS",login=Login)
				  message("GPS data downloaded successfully for ",nGPS," animals.")
				  flush.console()
 				  } 
  
if(exists("mbLoc_dfa")) {
  mbLoc_dfa<-mbLoc_dfa %>% 
    select(individual.local.identifier, timestamp, argos.lc, 
           location.lat, location.long, deployment.id, sensor.type.id)
}

if(exists("mbLoc_dfg")) {
  # Check if the lotek.crc.status.text column exists and subset based on it
  if("lotek.crc.status.text" %in% colnames(mbLoc_dfg)) {
  # Exclude rows where Lotek status == "OK(corrected)"
    mbLoc_dfg <- mbLoc_dfg %>% filter(.data[["lotek.crc.status.text"]] != "OK(corrected)")
    } else {
  # Otherwise, case where lotek.crc.status.text does not exist
    mbLoc_dfg <- mbLoc_dfg
  }
  mbLoc_dfg<-mbLoc_dfg %>% 
    mutate(argos.lc = 'G') %>%
    select(individual.local.identifier, timestamp, argos.lc, 
           location.lat, location.long, deployment.id, sensor.type.id)
}

### Exclude Argos (or GPS) if user requested....
cat("\n========================================================================================================\n")

if (exists("includeARGOS") && is.logical(includeARGOS) && length(includeARGOS) == 1 && 
    includeARGOS == FALSE && exists("mbLoc_dfa") ) {
  cat("\n>>>>>NOTE>>>>> Removing ARGOS locations per User's request: N=",nrow(mbLoc_dfa))
  cat("\n")
  rm(mbLoc_dfa)
}
if (exists("includeGPS") && is.logical(includeGPS) && length(includeGPS) == 1 && 
    includeGPS == FALSE && exists("mbLoc_dfg") ) {
  cat("\n>>>>>NOTE>>>>> Removing GPS locations per User's request: N=",nrow(mbLoc_dfg))
  cat("\n")
  rm(mbLoc_dfg)
}

#concatenate Argos and GPS if they both exist, otherwise pick the one that does....
if(exists("mbLoc_dfa") & exists("mbLoc_dfg")) {
   mbLoc_df <- rbind(mbLoc_dfg, mbLoc_dfa)
   }
if(exists("mbLoc_dfa") & !exists("mbLoc_dfg")) {
   mbLoc_df <- mbLoc_dfa
   }
if(!exists("mbLoc_dfa") & exists("mbLoc_dfg")) {
   mbLoc_df <- mbLoc_dfg
   }

### Drop variables with all missing data....
mbLoc_df <- mbLoc_df[,colSums(is.na(mbLoc_df))<nrow(mbLoc_df)]
mbRef_df <- mbRef_df[,colSums(is.na(mbRef_df))<nrow(mbRef_df)]

### Merge deployment attributes with each location data record....
data <- dplyr::left_join(mbLoc_df, mbRef_df, 
                         by=c("deployment.id" = "deployment_id", "sensor.type.id" = "sensor_type_id"))

### Remove any records with lat=0 AND lon=0 then sort chrnologically by animal....
data <- data %>%
  filter(!(abs(location.lat) <= 0.0001 & abs(location.long) <= 0.0001)) %>%
  arrange(individual.local.identifier,timestamp) 

### ===========================================================================================================
### Crosswalk critical variables....
### ===========================================================================================================

data <- data %>%
    rename(animal_id = individual.local.identifier,
          latitude  = location.lat,
		  longitude = location.long,
		  lc = argos.lc,
		  species = animal_taxon_canonical_name) %>%
   mutate(animal_id = as.character(animal_id)) %>%
   dplyr::select(animal_id,timestamp,latitude,longitude,lc,species) %>%
   na.omit() %>%
   arrange(animal_id, timestamp) %>%
   filter(n() >= 2) 
   
cat("\n========================================================================================================\n")
animalsIncluded <- table(data$animal_id)
cat("\nThese animal_ids (n locations) are staged for processing.\n")
print(animalsIncluded)
flush.console()
cat("\n========================================================================================================\n")

### ===========================================================================================================
### End of Movebank data ingestion....
### ===========================================================================================================

