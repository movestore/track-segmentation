

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
### Edit the following accordingly (DO NOT CHANGE VARIABLE NAMES)....
### ==========================================================================================================

### Define the existing directory where all the segmentation R scripts and example data files are located....
### Note, include the trailing "/"
sourceDir <- "C:/YOU_REPLACE_WITH_YOUR/PATH_HERE/"
setwd(sourceDir)

### Define any existing directory path where results will be written, include the trailing "/"
### This can be different or the same as the directory defined above.
resultsDir <- "C:/YOU_REPLACE_WITH_YOUR/PATH_HERE/"

### Define YOUR Movebank account login credentials....
myUser <- 'yourMovebankUser'
myPass <- 'yourMovebankPassword'

### Define the Movebank Study Name (exactly as it is in Movebank, including spaces, etc.)....
studyName <- "Galapagos Albatrosses"

### Some Movebank Studies contain both GPS locations and Argos locations.
### Define if you want to include (=TRUE) or exclude (=FALSE) either GPS or ARGOS location data....
### One or both can be set to TRUE. If both are FALSE, the script may crash.
includeGPS   <- TRUE
includeARGOS <- TRUE

### Do any tracks cross the -180/+180 longitude meridian, TRUE or FALSE?
Dateline  <- FALSE

### Define an animation frame-rate for the data visualization, units=hours....
frameRate <- 24   

### Would you like the option to visualize results for subsets of animals, TRUE or FALSE?
### If there are a lot of animals, and/or 10,000s locations in your data set,
###    then using subsetAnimals <- TRUE will let you pick which animals to visualize. This reduces
###    memory burdens and sluggishness when displayed in the html browser. Using subsetAnimals <- FALSE  
###    will skip the subsetting option altogether. 
subsetAnimals <- TRUE

### ==========================================================================================================
### Here the two thresholds that will define the spatial and temporal scales for detecting stops.
### ==========================================================================================================
### Define the maximum pairwise distance (Dist) among all locations during a stop (units = meters)....
proximity_distance_meters <- 150
### Define the minimum duration (Dur) for a stopover (units = hours)....
minimum_hours_threshold   <- 12
### ==========================================================================================================

### ==========================================================================================================
### Source this code ONCE to download and ingest all the GPS and/or Argos data in the Movebank Study....
### ==========================================================================================================
source("./coreMovebankDownloadCode_v01.R")
### ==========================================================================================================

### ==========================================================================================================
### Source this code ONCE to process all tracking data with the Dist and Dur thresholds defined above....
### This step will create the CSV output files.
### ==========================================================================================================
source("./coreStopoverCode_v01.R")
### ==========================================================================================================

### ==========================================================================================================
### If subsetAnimals <- TRUE, then source this code repeatedly to visualize different subsets of animals...
### (This visualization is only displaying the results that were generated in the step above.)
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
### (i.e., you don't need to rerun ("source") the Movebank download step).
### ==========================================================================================================



### ==========================================================================================================
### ANCILLARY EXAMPLE:
### This example uses all of the previously defined options except the five below. This analysis detects
### stops by a large sample of bar-tailed godwits. A broad spatial tolerance is defined (20 km) to
### accommodate Argos location errors, and a short duration is defined to accommodate restrictive duty cycles
### that were imposed to conserve battery life.
### ==========================================================================================================
studyName <- "barTailedGodwit_USGS_ASC_argos"
Dateline  <- TRUE
subsetAnimals <- FALSE
### ==========================================================================================================
### Define the maximum pairwise distance (Dist) among all locations during a stop (units = meters)....
proximity_distance_meters <- 20000
### Define the minimum duration (Dur) for a stopover (units = hours)....
minimum_hours_threshold   <- 3
### ==========================================================================================================
source("./coreMovebankDownloadCode_v01.R")
source("./coreStopoverCode_v01.R")
source("./coreVisualizeCode_v01.R")
### ==========================================================================================================



