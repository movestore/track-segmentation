# Track Segmentation

MoveApps

Github repository: *github.com/movestore/track-segmentation*

## Description

Segment a time series of location data for individual animals into sequential
periods of stops and movements based on user-provided location proximity and
stop duration thresholds. Explore the results in an interactive map.

## Documentation

This app applies a track segmentation algorithm to the locations in the input
data to identify stop locations. Within the app, the user defines two
input parameters:

-   a maximum allowable pairwise distance (referred to as `proximity`) among all
    locations that constitute a single stop (in meters).
-   a minimum required duration (referred to as `min_hours`) that a stop 
    must persist (in hours).

### Using the app

To run the segmentation algorithm, enter a value in the 
**Maximum distance between stopped locations (meters)** and 
**Minimum stop duration (hours)** fields, then click the 
**Identify stop locations** button.

The app will be inactive while the algorithm runs. When it finishes, you will
be able to see the results on the interactive map in the app. Use the
**Time range of interest** slider to focus on the results for a particular 
time range. 

If your data are very dense, you can also select the "Thin map
points" option to display a smaller subset of the tracked locations on the map.
You can specify the number of points to include per animal in the 
"Max points to plot (per individual)" box.

You can also click on the stop and metastop points to view metadata
about those points and show/hide specific individuals using the panel on the
right side of the map.

Click on the "Results" tab to see the identified stop and metastop locations
in tabular format.

After the algorithm has been run for a set of inputs, click the "Write results"
button to write the stop and metastop location results in CSV format as
app artifact files.

Details about the algorithm can be found in the "App Details" tab and in this
README.

### Algorithm details

#### Part 1: Identifying stops

The algorithm steps chronologically through each animal's tracking time
series and identifies sequences of locations (i.e., periods of time)
when the animal was stopped (or conversely, not stopped). Locations that
constitute a **stop** are defined as those that:

1.  are all within `proximity` from one another (i.e., distances between
    all pairwise locations will be less than `proximity`).
2.  have an elapsed time between the first and last location that is
    greater than `min_hours`.

It follows that stops must consist of at least two distinct locations.

The algorithm begins with the first location for a given animal (the anchor 
location) and calculates the distance and elapsed time to the next location. If
the distance is greater than `proximity` no stop is detected and the
procedure repeats with the next location as the anchor.

If the distance is less than `proximity` and the elapsed time is less
than `min_hours` then a “potential stop” is detected and the next
location is considered. If all three pairwise distances between the
three locations are less than `proximity` but the elapsed time from the
first to the third location is still less than `min_hours`, then the
stop remains “potential” and the next location is considered. Locations
are sequentially added to the “potential” stop until **either**:

1.  all pairwise distances among the locations are less than `proximity`
    and the elapsed time from the first to the last location is greater
    than `min_hours`. In this case a stop is detected. The stop will
    include all locations from the anchor location to the final location
    that did not exceed the `proximity` threshold. The location that
    exceeded the threshold then becomes the new anchor location, and the
    algorithm continues.
2.  one or more pairwise distances are greater than `proximity`. In this
    case no stop is detected. The location that exceeded the threshold
    becomes the new anchor location, and the algorithm continues.

After all locations for a given animal have been processed, a single
location for each detected stop is calculated as a weighted average of
the respective stop’s locations. The weighting gives strong emphasis on
GPS locations, followed by emphasis on higher quality Argos locations
compared to lower quality Argos locations.

These derived stop locations are assigned a timestamp equivalent to the
respective stop’s initial (anchor) tracking location. Note that “stop
locations” are derived averages of the tracking location data (i.e.,
they are *not* observed data).

#### Part 2: Identifying metastops

A **metastop** consists of one or more chronologically sequential stops
in which all pairwise distances between the stop locations are less than
`proximity`.

To identify metastops, the algorithm runs the same procedure as
described in part 1, but uses the *derived stop locations* (and
timestamps) as the inputs to the algorithm instead of the raw location
data.

Depending on the provided `proximity` and `min_hours` thresholds and the
precision of the location data (which affects the weighted location
averaging), consecutive discrete stops can be less than `proximity`
meters apart. By identifying metastops, the segmentation algorithm combines 
information from adjacent stops while also preserving the identity of 
each unique stop.

Unlike stop locations, metastops may consist of only one stop, in which
case the metastop and the single stop share the same attributes. This
ensures that when considered in full, the metastop results comprehensively 
include all identified stops. When a metastop comprises more than
one stop, the location of the
stop with the longest duration (i.e., where the animal spent the most
time) is used as the recorded metastop location. The timestamp of the
earliest stop in the metastop is used as the metastop's recorded timestamp.

### Results

After the algorithm completes, the classified stop, metastop, and movement 
locations are displayed on an interactive leaflet map.

Metastop location markers are displayed in red and are scaled in size relative
to the number of stops that belong to that metastop.

Other points are classified as either "Stopped", "Movement during stop", 
or "Movement". Note that those points classified as "Stopped" represent points
from the *annotated location data* for the animal; these points do not show
the *derived* stop locations used to identify metastops.

The classified locations are also summarized in a tabular format in the 
app's "Results" tab. 

After running the algorithm, the results of the classified stop, 
metastop, and movement locations can be saved as an app artifact by clicking
the "Write results" button.

### Application scope
#### Generality of App usability

This App was developed for any taxonomic group. 

#### Required data properties

The App should work for any kind of location data.

The segmentation algorithm provides some handling specifically designed 
for Argos data, but location data collected by other means are also fully 
supported.

### Input type

`move2::move2_loc`

### Output type

`move2::move2_loc`

### Artefacts

`track_segmentation_v014_proxMeters{proximity}_minHours{min_hours}.zip`:
contains three .csv files with the results of the segmentation algorithm for
the indicated `proximity` and `min_hours` parameters.

-   `stopovers_v014_proxMeters{proximity}_minHours{min_hours}.csv`: contains 
    one record for each identified stop, including animal identifier, stop
    identifier, start and end time for the stop, weighted location of the stop,
    and the number of recorded locations that belong to that stop.

-   `metastops_v014_proxMeters{proximity}_minHours{min_hours}.csv`:
    contains one record for each metastop, including animal identifier, metastop
    identifier, start and end time for the metastop, location of the metastop,
    and the number of identified stops that belong to that metastop.

-   `locationsAnnotated_v014_proxMeters{proximity}_minHours{min_hours}.csv`: 
    contains one record for each input location annotated with a variable 
    denoting the animal's status at the respective place and time (either 
    "Stopped", "Movement", or "Movement during stop"). For locations classified
    as "Stopped", the associated stop and metastop ids are also indicated.

### Settings 

**Maximum distance between stopped locations (meters)** (`proximity`): the 
maximum allowable pairwise distance (`proximity`) among all locations that 
constitute a single stop (in meters).

**Minimum stop duration (hours)** (`min_hours`): the minimum required duration 
(`min_hours`) that a stop must persist (in hours).

**Time range of interest** (`time_range`): Filter the output map to show only
those locations that fall within a given time interval. This does not affect
the results of the segmentation algorithm, but can be used for targeted
exploration of the segmentation results.

**Store settings**: Click to store the current settings of the App for future 
Workflow runs. 

### Changes in output data

The input data remain unchanged.

### Most common errors

Input data sources with large numbers of records can cause the interactive
map in the app to lag, sometimes substantially. In these cases, you can
use the checkboxes on the right side of the map to hide certain individuals from
the map, which can improve map reactivity. Note that hiding layers from the map
is purely for visual exploration purposes and does not remove these individuals
from the segmentation calculations or the output results files.

To run the segmentation algorithm for a specific subset of tracks, you
must filter your data to those tracks prior to launching this app.

### Null or error handling

If the provided distance and stop duration parameters do not identify any
stops, all locations will be classified as movement locations and no results
will be shown in the "Results" tab.
