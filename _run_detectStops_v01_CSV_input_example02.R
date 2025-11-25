

######################################################################################################
######################################################################################################
# Developed in R 4.4.1, by ddouglas@usgs.gov, May 2025
######################################################################################################
######################################################################################################

# Install (if necessary) then load required packages
if (!requireNamespace("geosphere", quietly = TRUE)) install.packages("geosphere")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("move", quietly = TRUE)) install.packages("move")
if (!requireNamespace("geosphere", quietly = TRUE)) install.packages("geosphere")
if (!requireNamespace("shiny",     quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("RColorBrewer",     quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")
if (!requireNamespace("leaflet.extras", quietly = TRUE)) install.packages("leaflet.extras")
if (!requireNamespace("leaflet.extras2", quietly = TRUE)) install.packages("leaflet.extras2")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("geojsonsf", quietly = TRUE)) install.packages("geojsonsf")
if (!requireNamespace("circular", quietly = TRUE)) install.packages("circular")
if (!requireNamespace("raster", quietly = TRUE)) install.packages("raster")
if (!requireNamespace("leafem", quietly = TRUE)) install.packages("leafem")

library(lubridate)
library(tidyr)
library(move)
library(geosphere)
library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(htmlwidgets)
library(leaflet.extras2)
library(sf)
library(geojsonsf)
library(leaflet.extras)
library(circular)
library(leafem)
library(raster)

# Clear all objects from the environment....
rm(list = ls())

### ==========================================================================================================
### ==========================================================================================================
### ==========================================================================================================
### ==========================================================================================================
### ==========================================================================================================
### These are the 6 required input variables, named exactly this way:
### 1. animal_id	(character)		-unique character string for each animal
### 2. timestamp 	(POSIXct)		-R date-time variable
### 3. latitude 	(numeric)		-latitude (-90 to 90)
### 4. longitude 	(numeric)		-longitude (-180 to 180)
### 5. lc			(character)		-single character Argos location class (3,2,1,0,A,B,Z), or "G" for GPS
### 6. species 		(character)		-species or common name that will be included in output data sets
### ==========================================================================================================
### Edit the following accordingly....
### ==========================================================================================================

### Define the existing directory where all the segmentation R scripts and example data files are located....
sourceDir <- "C:/YOU_REPLACE_WITH_YOUR/PATH_HERE/"
setwd(sourceDir)

### Define an existing directory path were results will be written, include the trailing "/"
### Can be different or the same as the directory defined above.
resultsDir <- "C:/YOU_REPLACE_WITH_YOUR/PATH_HERE/"

### Define your input CSV file....
indat <- read.csv(file="./inputData_example02.csv")

### Define an arbitrary Study Name (will be embedded into the CSV output file names)....
studyName <- "example02_GPS" 

### Do any locations cross the -180/+180 longitude meridian, TRUE or FALSE?
Dateline  <- FALSE

### Define an animation frame-rate for the data visualization, units=hours....
frameRate <- 2 

### Would you like the option to display results for subsets of animals, TRUE or FALSE?
### If there are a lot of animals, and/or 10,000s locations in your data set,
###    then using subsetAnimals <- TRUE will let you pick which animals to visualize, which reduces
###    memory burdens and sluggishness when displayed throughthe html browser. Using subsetAnimals <- TRUE  
###    will skip the subsetting option altogether. 
subsetAnimals <- FALSE

### Here, wrangel the raw input data so all required variables (see above) are present in 'data',
### remove records with missing data, sort chronologically by animal_id, and group by animal_id.
data <- indat %>%
  mutate(timestamp=as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")) %>%
  mutate(latitude=gps_latitude, longitude=gps_longitude) %>%
  mutate(lc='G') %>%
  select(animal_id,timestamp,latitude,longitude,lc,species) %>%
  na.omit() %>%
  arrange(animal_id, timestamp) %>%
  group_by(animal_id)


### ==========================================================================================================
### These are the two thresholds that define the spatial and temporal scales for detecting stops.
### ==========================================================================================================
### Define the maximum pairwise distance (Dist) among all locations during a stop (units = meters)....
proximity_distance_meters <- 100
### Define the minimum duration (Dur) for a stopover (units = hours)....
minimum_hours_threshold   <- 3
### ==========================================================================================================

### ==========================================================================================================
### Source this code ONCE to process all data with Dist and Dur (above), and write the 3 output CSV files....
### ==========================================================================================================
source("./coreStopoverCode_v01.R")
### ==========================================================================================================

### ==========================================================================================================
### If subsetAnimals <- TRUE, source this code repeatedly to visualize different subsets of animals...
### Running this code (with subsetAnimals <- TRUE) will open an interactive HTML browser where users can
### select animal(s) for visualization, afterwhich the map visualization is displayed. 
### The map visualization is terminated by clicking the "STOP ICON" in the R console. 
### ==========================================================================================================
source("./coreVisualizeCode_v01.R")
### ==========================================================================================================


### ==========================================================================================================
### END OF SCRIPT
###
### The last statement can be rerun repeatedly to visualize different animal subsets (when subsetAnimals<-TRUE).
###
### If you want to change the Dist and/or Dur thresholds, do so and then rerun the last two source statements
### (i.e., you don't need to rerun ("source") the Movebank download step).  See examples below. 
### NOTE: You must "STOP" the HTML map visualization (using the STOP icon in the R console) before running each 
### example below. 
### ==========================================================================================================




### ==========================================================================================================
### Ancillary examples:
### ==========================================================================================================
proximity_distance_meters <- 1500
minimum_hours_threshold   <- 3
source("./coreStopoverCode_v01.R")
source("./coreVisualizeCode_v01.R")
### ==========================================================================================================


### ==========================================================================================================
proximity_distance_meters <- 100
minimum_hours_threshold   <- 10
source("./coreStopoverCode_v01.R")
source("./coreVisualizeCode_v01.R")
### ==========================================================================================================













