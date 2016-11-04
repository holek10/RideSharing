# Ridesharing with your friends from work

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

This repository contains the source code for an interactive app that allows for finding a friend from your work who can give you a lift there and back. Obviosuly the concept can be extended to any environment and adjusted to fit the purpose.

## About
The app is built using [R](http://www.r-project.org) and [Shiny](http://shiny.rstudio.com) web framework , utilizing [Leaflet](http://www.leafletjs.com) maps and [Leaflet Routing Machine](http://www.liedman.net/leaflet-routing-machine/) with custom tile from [Mapbox] (http://www.mapbox.com).

## Data source
Currently the dummy data are used and stored locally in *.rds* file.  
Refer to [this guideline](http://deanattali.com/blog/shiny-persistent-data-storage/) on how to store data with Shiny applications. 

## The idea behind
In one sentence: show 3 nearest routes from home to work compared to my route.  

Location of end point (office, work address) is already fixed on the map - 'Company headquarter'. A user is supposed to enter his/her home location (address) to automatically plot the optimal route. The route should be then adjusted to reflect true way of communiting between home and work.  Every adjustment adds new waypoint which is valuable for determining the nearest route. 

The nearest route is minimum distance between home location (latitude and longitude) and routes (waypoints) of all other users in the database. The default metric is Euclidean distance, which can be changed to Manhattan, Quadrance or ordinary mathematical difference.
```R
calculateDistance <- function(latLng, waypoints, method = "euclidean") {
  # calculate difference between two locations (home and all other stored route waypoints)
  waypointsDim <- dim(waypoints)[1]
  difference <- matrix(NA, nrow = waypointsDim, ncol = 2)
  for (i in c(1:waypointsDim) ) {
    difference[i,] <- latLng - waypoints[i,]
  }
  # select method for calculating the distance (default: Euclidean distance)
  if (method == "euclidean") minDistance <- min(sqrt(rowSums(difference^2)))
  else if (method == "manhattan") minDistance <- min(rowSums(abs(difference)))
  else if (method == "quadrance") minDistance <- min(rowSums(difference^2))
  else if (method == "differeces") minDistance <- min(difference) 
  else stop(sprint("Unknown method: %s", method))
  return(minDistance)
}
```
## How to run the app
> Online version is now up and running here: http://holek10.shinyapps.io/Ridesharing  

To run the app locally you should install required packages: **shiny**,  **RJSONIO**, and **RCurl** in R, for example: 
```R
if (!require('shiny')) install.packages("shiny")
```
and use the function `runGithub()` with specified repository name under my username:
```R
shiny::runGitHub("Ridesharing", "holek10")
```
## Licensing 
The app is provided for free under GNU General Public License
