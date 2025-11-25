# Track Segmentation: Stops and Movements
 - release v1.0.0
      - a newer version of this software package may be available.
        - provisional updates: https://code.usgs.gov/asc/track-segmentation/-/tree/main
        - approved releases: https://code.usgs.gov/asc/track-segmentation/-/releases

## Author
David C. Douglas (ORCID: 0000-0003-0186-1104) U.S. Geological Survey - Alaska Science Center

## Suggested Citation
Douglas, D.C., 2025. Track segmentation: stops and movements (ver 1.0.0, July 2025): U.S. Geological Survey software release, https://doi.org/10.5066/P13QURM9

## Contact
David C. Douglas  ddouglas@usgs.gov
- U.S. Geological Survey - Alaska Science Center;
 4210 University Drive;
 Anchorage, Alaska 99508 USA;
 907-786-7000
 gs-ak_asc_datamanagers@usgs.gov

## Project Overview
   - This R script inputs animal tracking data and segments the time series for each individual animal into sequential periods of stops and movements. The algorithm identifies stops based on two influential user-defined thresholds: 1) a distance threshold (hereafter 'Dist') that defines the upper limit of spatial displacement permissible while occupying a stop, and 2) a duration threshold (hereafter 'Dur') that defines the minimum amount of time a stop is required to persist. Values for the Dist and Dur should be chosen in a manner that targets animal behavior(s) of interest while also accommodating the spatial accuracy and temporal frequency of the location data under analysis. Locations not classified as stops are classified as movements. A "User Guide" is included in the software package.

## Software Requirements
 - Scripts were developed in R (version 4.4.1, and tested in version
    4.4.2)
    - available for free download from the Comprehensive R Archive Network (CRAN) https://cran.r-project.org

## Files Overview

### R scripts:
 - `coreVisualizeCode_v01.R`	- displays results in an interactive map
 - `coreStopoverCode_v01.R`	- generates results based user's chosen options
 - `coreMovebankDownloadCode_v01.R`	- acquires input data from Movebank.org
 - `_run_detectStops_v01_Movebank_input_example01.R`	- an example script for analysis of tracking data stored at Movebank.org
 - `_run_detectStops_v01_CSV_input_example01.R`	- example script #1 for analysis of tracking data stored in tabular CSV format
 - `_run_detectStops_v01_CSV_input_example02.R`	- example script #2 for analysis of tracking data stored in tabular CSV format

### Example data:
- `inputData_example01.csv`		- tracking data in CSV format for example script #1
- `inputData_example02.csv`		- tracking data in CSV format for example script #2

### Other software release files
- `User_Manual_Track_Segmentation.pdf`		- User manual
- `README.md`		- documentation for this USGS software release
- `code.json`		- metadata for this USGS software release 
- `LICENSE.md`		- standard USGS software release license and copyright
- `DISCLAIMER.md`	- standard USGS software release disclaimer
- `CHANGELOG.md`	- package revision history

## Distribution
- The U.S. Geological Survey is the authoritative source and distributor of this software release through this repository https://doi.org/10.5066/P13QURM9

