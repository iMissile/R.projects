# rm(list=ls()) # очистим все переменные

library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(sp)       # spatial operations library
library(leaflet)
library(KernSmooth)
library(akima)
library(rgl)

options(warn=1) # http://stackoverflow.com/questions/11239428/how-to-change-warn-setting-in-r
# options(viewer = NULL) # send output directly to browser


ifilename <- ".\\data\\sensors_zoo.csv"
ifilename <- ".\\data\\sensors_calc.csv"
ifilename <- ".\\data\\sensors_manual.csv"
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
# ƒобавим колонку принудительно. јктуально дл€ загрузки расчетных позиций сенсоров
mydata$val <- runif(nrow(mydata), 10, 90) # накидываем влажность в процентах

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
# m <- leaflet(mydata, base.map="osm") #setup map
m <- leaflet(mydata) %>% #setup map
  addTiles() %>% #add open street map data
  setView(lng = 37.58, lat = 55.76, zoom = 15) %>%
  addMarkers(lng = ~lon, lat = ~lat)
# map$tileLayer(provider = 'Stamen.Watercolor') # https://github.com/ramnathv/rMaps
#m$tileLayer(provider = 'Stamen.Watercolor')

#m <- addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
# имена провайдеров смотрим в т.ч. здесь: http://leaflet-extras.github.io/leaflet-providers/preview/
# m <- addProviderTiles(m, "CartoDB.Positron")
#m <- addProviderTiles(m, "Stamen.Watercolor")
# m <- addProviderTiles(m, "OpenWeatherMap.RainClassic")


m


stop()
# теперь по новой, берем идеи отсюда: http://stackoverflow.com/questions/24410292/how-to-improve-interp-with-akima

fld <- with(mydata, interp(lon, lat, val,
    xo = seq(min(lon), max(lon), length = 500),
    yo = seq(min(lat), max(lat), length = 500)
  )) # http://www.statmethods.net/stats/withby.html
filled.contour(x=fld$x, y=fld$y, z=fld$z,
               color.palette=colorRampPalette(c("white", "blue")))

stop()

# http://docs.ggplot2.org/0.9.3.1/stat_contour.html
# Basic plot
v <- ggplot(mydata, aes(x = lon, y = lat, z = val))
# v + stat_contour(bins = 5)
v

qplot(lon, lat, z = val, data = mydata, geom = "contour")
stop()


s <-  interp(mydata$lon, mydata$lat, mydata$val, linear = FALSE, extrap = TRUE) #from akima
# If linear is TRUE (default), linear interpolation is used in the triangles bounded by data points.
# Cubic interpolation is done if linear is set to FALSE. If extrap is FALSE, z-values for points outside
# the convex hull are returned as NA. No extrapolation can be performed for the linear case.
mm <- s$z
# contour(s$x, s$y, s$z)
contour(s)

CL <- contourLines(s$x, s$y, s$z, nlevels = 9)
# nlevels указывает разбиение, но реально нарезок может быть больше (несколько гор)

m = leaflet() %>% addTiles() %>%
  # setView(lng = 37.58, lat = 55.76, zoom = 14) %>%
  addPolygons(CL[[1]]$x,CL[[1]]$y,fillColor = "red", stroke = FALSE) %>%
  addPolygons(CL[[3]]$x,CL[[3]]$y,fillColor = "green", stroke = FALSE) %>%
  addPolygons(CL[[5]]$x,CL[[5]]$y,fillColor = "blue", stroke = FALSE) %>%
  addPolygons(CL[[7]]$x,CL[[7]]$y,fillColor = "red", stroke = FALSE) %>%
  addPolygons(CL[[9]]$x,CL[[9]]$y,fillColor = "red", stroke = FALSE)

w <- s$z

m

stop()

# http://freakonometrics.hypotheses.org/19473
# http://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package
X <- mydata[c("lat", "lon")]
# bw.ucv -- Bandwidth Selectors for Kernel Density Estimation
kde2d <- bkde2D(X, bandwidth = c(bw.ucv(X[,1]), bw.ucv(X[,2])))
# bkde2d не очень годитс€, поскольку это density estimate

t <- kde2d$fhat

contour(x=kde2d$x1, y=kde2d$x2, z=kde2d$fhat)
stop()
data(quakes)

