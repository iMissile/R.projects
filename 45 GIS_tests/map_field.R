# rm(list=ls()) # очистим все переменные

library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(sp)       # spatial operations library
library(leaflet)

options(warn=1) # http://stackoverflow.com/questions/11239428/how-to-change-warn-setting-in-r


ifilename <- ".\\data\\sensors.csv"
# подгружаем данные по сенсорам
mydata <- read_delim(ifilename, delim = ",", quote = "\"",
                     col_names = TRUE,
                     locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
                     # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")), 
                     progress = interactive()
) # http://barryrowlingson.github.io/hadleyverse/#5

## Load the sensor positions.
# mydata <- read.csv(ifilename, header=T, stringsAsFactors = FALSE)
# данные должны быть в формате data.frame
# иначе SpatialPointsDataFrame ругаетс€: unable to find an inherited method 
# for function СcoordinatesТ for signature С"tbl_df"Т

mydata <- as.data.frame(mydata)


# —начала надо преобразовать в Spatial Data
# 1. ќпредел€ем CRS
dCRS <- CRS("+init=epsg:4326")
# http://www.r-tutor.com/r-introduction/data-frame/data-frame-column-slice
mydataSp <- SpatialPointsDataFrame(mydata[c('lon', 'lat')], mydata, proj4string = dCRS)

# plot spatial object
plot(mydataSp, main = '—енсоры на поле')



# =================== leaflet ==================
# дл€ leaflet необ€зательна работа с Spatial Objects
#build the map without "pipes"
m <- leaflet(mydata, base.map="water") #setup map
m <- addTiles(m) #add open street map data
m <- setView(m, lng = 38.6, lat = 54.9, zoom = 12)
# map$tileLayer(provider = 'Stamen.Watercolor') # https://github.com/ramnathv/rMaps
#m$tileLayer(provider = 'Stamen.Watercolor')

#m <- addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)

m
