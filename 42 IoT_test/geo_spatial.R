library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(rgdal)    # GDAL bindings for loading GPX-data
library(sp)       # spatial operations library

# центр поля: 54.860510, 38.655286

## Load the sensor positions.
sensors <- read.csv("sensors.csv", header=T)

## Inspect column headings
head(sensors)

## Plot the XY coordinates (do not close the plot window).
plot(sensors$long, sensors$lat)

## We can tell R that "sensors" contains spatial data with the coordinates function
coordinates(sensors) <- c("long", "lat")

## Have a look at the created object
class(sensors)
str(sensors)

#make location_information a spatial points shapefile 
#get lat/long from location_information 
xy2<-location_information[,c(2,3)] 
ll_points <- SpatialPointsDataFrame(coords=xy2,data=location_information,proj4string =CRS("+proj=longlat + ellps=WGS84")) summary(ll_points) writeOGR(ll_points, dsn="N:/2015 Projects/Surveys/Station Allocation/gpxfile.gpx",layer="waypoints3",driver="GPX")