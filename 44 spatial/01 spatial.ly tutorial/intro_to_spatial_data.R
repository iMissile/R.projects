##Code (Comments are preceded by ##)

##Handling spatial data with R

##This tutorial provides an introduction to handling spatial data with R. It outlines how to create a simple spatial object, load a shapefile and add/ change projection information. It is pretty dry, but provides important foundations to the more interesting things you can do with spatial data using R. 

##Data Requirements:

##London Sport Participation Shapefile, London Cycle Hire locations.

##Install the following packages (if you haven't already done so):

##maptools, rgdal

## Load required packages
library(maptools)
library(rgdal)

##Set your working directory- this should be the folder where the above files are saved.

setwd("/XX/XX")

## Load the cycle hire  locations.

cycle<- read.csv("London_cycle_hire_locs.csv", header=T)

## Inspect column headings

head(cycle)

## Plot the XY coordinates (do not close the plot window).

plot(cycle$X, cycle$Y)
## We can tell R that "cycle" contains spatial data with the coordinates function

coordinates(cycle)<- c("X", "Y")

## Have a look at the created object

class(cycle)
str(cycle)

## The class has become a "SpatialPointsDataFrame" which is a type of S4 object that requires handling slightly differently. The str() output contains lots of @ symbols which denote a different slot. Typing cycle@data will extract the attribute data. The X and Y locational information can now be found in the coords slot. In addition bbox contains the bounding box coordinates and the pro4string contains the projection, or CRS (coordinate reference system) information. We have not specified this so it is empty at the moment. We therefore need to refer to the correct Proj4 string information. These are loaded with the rgdal package and can simply be referred to with an ID. To see the available CRSs enter:

EPSG<- make_EPSG()
head(EPSG)

##In this case the data are in British National Grid. We can search for this within the EPSG object.
with(EPSG, EPSG[grep("British National", note),])

## The code we are after is 27700. 

BNG<- CRS("+init=epsg:27700")

proj4string(cycle)<-BNG

## From this point we can combine the data with other spatial information and also perform transformations on the data. This is especially useful when exporting to other software. See the XXX tutorial. 

## Shapefiles are extremely simple to import/ export to/from an R session and are handled as spatial objects in the same way as above. In this case we are going to load in a SpatialPolygonsDataframe. We can specify the CRS when the data at this stage (it is BNG as above).

sport<- readShapePoly("london_sport.shp", proj4string= BNG)

##have a look at the attribute table headings (these are the values stored in the @data slot)

names(sport)

## and you can double check the CRS:

sport@proj4string

## and of course plot the data

plot(sport, col="blue")

## add the cycle points to check they line up (they should be around the centre of London).

plot(cycle, add=T, col= "red", pch=15)

## Refer to other tutorials if you want to produce interesting maps with these data.
## To export the data as a shapefile use the following syntax for the point data:

writePointsShape(cycle, "cycle.shp")

## and the polygon data (replace Poly with Lines for spatial lines)

writePolyShape(sport, "london_sport.shp")

##Done!

##Acknowledgements:

##Much of this content was adapted from a worksheet written by Dr Gavin Simpson (UCL Geography). 

##Further Reading:

##Applied spatial data analysis with R. Bivand et al.


##Disclaimer: The methods provided here may not be the best solutions, 
##just the ones I happen to know about! No support is provided with these worksheets. 
##I have tried to make them as self-explanatory as possible and will not be able to 
##respond to specific requests for help. I do however welcome feedback on the tutorials.
##License: cc-by-nc-sa. Contact: james@spatialanalysis.co.uk
