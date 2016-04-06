library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(rgdal)    # GDAL bindings for loading GPX-data
# library(raster)
library(sp)       # spatial operations library

# Coordinate Reference Systems (CRS) центр поля: 54.860510, 38.655286 Туториал
# взят отсюда:
# http://data-lessons.github.io/NEON-R-Spatial-Vector/R/csv-to-shapefile-R/ 
# In R, the notation used to describe the CRS is proj4string from the PROJ.4 library. 
# It looks like this: +init=epsg:4121 +proj=longlat +ellps=GRS80
# +datum=GGRS87 +no_defs +towgs84=-199.87,74.79,246.62

# WGS84 (EPSG:4326)
# http://spatialreference.org/ref/sr-org/google-projection/proj4/
# +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs

## Load the sensor positions.
sensors <- read.csv("sensors.csv", header=T, stringsAsFactors = FALSE)

## Inspect column headings
head(sensors)

## Plot the XY coordinates (do not close the plot window).
plot(sensors$long, sensors$lat)

# Сначала надо преобразовать в Spatial Data

# Определяем CRS
sensorsCRS <- CRS("+init=epsg:4326")

# http://www.r-tutor.com/r-introduction/data-frame/data-frame-column-slice
sensorsSp <- SpatialPointsDataFrame(sensors[c('long', 'lat')], sensors, proj4string = sensorsCRS)

# To assign a known CRS to spatial data: proj4string(x) <- CRS("+init=epsg:28992")
# google
# proj4string(x) <- CRS("+init=epsg:4326")

# plot spatial object
plot(sensorsSp, main = 'Сенсоры на поле')


# writeOGR(sensorsSp ,dsn = "sensors.shp", layer = "world_test", driver = "ESRI Shapefile", overwrite_layer = TRUE)

# Error using writeOGR {rgdal} to write gpx file
# http://stackoverflow.com/questions/25064823/error-using-writeogr-rgdal-to-write-gpx-file
# R writeOGR GPX "Creating Name field failed" -- переименовал layer в "waypoints" и заработало

# сохраняем файл в формате GPX
# http://zevross.com/blog/2016/01/13/tips-for-reading-spatial-files-into-r-with-rgdal/
writeOGR(
  sensorsSp,
  dsn = "sensors_out.gpx",
  layer = "waypoints",
  driver = "GPX",
  overwrite_layer = TRUE,
  dataset_options = "GPX_USE_EXTENSIONS=yes"
)

stop()

tSp <- readOGR()
## We can tell R that "sensors" contains spatial data with the coordinates function
coordinates(sensors) <- c("long", "lat")

## Have a look at the created object
class(sensors)
str(sensors)


ll_points <-
  SpatialPointsDataFrame(
    coords = xy2,
    data = location_information,
    proj4string = CRS("+proj=longlat + ellps=WGS84")
  )
summary(ll_points)
writeOGR(ll_points,
         dsn = "N:/2015 Projects/Surveys/Station Allocation/gpxfile.gpx",
         layer = "waypoints3",
         driver = "GPX")

#make location_information a spatial points shapefile 
#get lat/long from location_information 
xy2 <- location_information[, c(2, 3)]
ll_points <-
  SpatialPointsDataFrame(
    coords = xy2,
    data = location_information,
    proj4string = CRS("+proj=longlat + ellps=WGS84")
  )
summary(ll_points)
writeOGR(ll_points,
         dsn = "N:/2015 Projects/Surveys/Station Allocation/gpxfile.gpx",
         layer = "waypoints3",
         driver = "GPX")