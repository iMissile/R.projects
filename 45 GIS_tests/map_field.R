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
library(RColorBrewer)

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
# иначе SpatialPointsDataFrame ругается: unable to find an inherited method 
# for function ‘coordinates’ for signature ‘"tbl_df"’

mydata <- as.data.frame(mydata)
# Добавим колонку принудительно. Актуально для загрузки расчетных позиций сенсоров
mydata$val <- runif(nrow(mydata), 10, 90) # накидываем влажность в процентах

# Сначала надо преобразовать в Spatial Data
# 1. Определяем CRS
dCRS <- CRS("+init=epsg:4326")
# http://www.r-tutor.com/r-introduction/data-frame/data-frame-column-slice
mydataSp <- SpatialPointsDataFrame(mydata[c('lon', 'lat')], mydata, proj4string = dCRS)

# plot spatial object
plot(mydataSp, main = 'Сенсоры на поле')



# =================== leaflet ==================
# для leaflet необязательна работа с Spatial Objects
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
# =============================================
# Для статических дашбордов достаточно фиксированных растров
# https://github.com/dkahle/ggmap
# www.unomaha.edu/mahbubulmajumder/data-science/fall-2014/lectures/06-display-spatial-data/06-display-spatial-data.html
library(ggmap)

fmap <-
  get_map(
    enc2utf8("Москва, Зоологическая 2"),
    language = "ru-RU",
    source = "stamen",
    maptype = "watercolor", 
    #maptype = "terrain",
    zoom = 16
  )
# source = "osm",

mydata$val <- runif(nrow(mydata), 30, 80) # накидываем влажность в процентах

# данных крайне мало, чтобы не было сильных перепадов принудительно раскидаем 
# по периметру прямоугольника сенсоры с минимальным значением влажности. (мы там не поливаем)
local({
  dlon <- (max(mydata$lon) - min(mydata$lon)) * 0.2
  dlat <- (max(mydata$lat) - min(mydata$lat)) * 0.2
  
  hdata <- data.frame(expand.grid(
    lon = seq(min(mydata$lon) - dlon, max(mydata$lon) + dlon, length = 10),
    lat = c(min(mydata$lat) - dlat, max(mydata$lat) + dlat),
    val = min(mydata$val)
  ))
  
  vdata <- data.frame(expand.grid(
    lon = c(min(mydata$lon) - dlon, max(mydata$lon) + dlon),
    lat = seq(min(mydata$lat) - dlat, max(mydata$lat) + dlat, length = 10),
    val = min(mydata$val)
  ))
})

# сделаем периметр по размеру прямоугольника отображения карты
hdata <- data.frame(expand.grid(
  lon = seq(attr(fmap,"bb")$ll.lon, attr(fmap,"bb")$ur.lon, length = 10),
  lat = c(attr(fmap,"bb")$ll.lat, attr(fmap,"bb")$ur.lat),
  val = min(mydata$val)
))

vdata <- data.frame(expand.grid(
  lon = c(attr(fmap,"bb")$ll.lon, attr(fmap,"bb")$ur.lon),
  lat = seq(attr(fmap,"bb")$ll.lat, attr(fmap,"bb")$ur.lat, length = 10),
  val = min(mydata$val)
))


tdata <- rbind(mydata, hdata, vdata)
# mydata <- tdata
# теперь готовим матрицу для градиентной заливки
# берем идеи отсюда: http://stackoverflow.com/questions/24410292/how-to-improve-interp-with-akima
# и отсюда: http://www.kevjohnson.org/making-maps-in-r-part-2/
fld <- interp(tdata$lon, tdata$lat, tdata$val,
              xo = seq(min(tdata$lon), max(tdata$lon), length = 200),
              yo = seq(min(tdata$lat), max(tdata$lat), length = 200),
              duplicate = "mean", # дубликаты возникают по углам искуственного прямоугольника
              #linear = TRUE, #FALSE (после того, как добавили внешний прямоугольник, можно)
              linear = FALSE,
              extrap = TRUE)

# превращаем в таблицу значений для комбинаций (x, y)
# хранение колоночного типа, адресация (x, y) 
# поэтому для делается хитрая развертка -- бегущий x раскладывается по фиксированным y, как оно хранится
dInterp <- data.frame(expand.grid(x = fld$x, y = fld$y), z = c(fld$z)) 
# при моделировании сплайнами, 
# в случае крайне разреженных данных могут быть косяки со слишком кривыми аппроксимациями
# dInterp$z[dInterp$z < min(mydata$val)] <- min(mydata$val)
# dInterp$z[dInterp$z > max(mydata$val)] <- max(mydata$val)
dInterp$z[is.nan(dInterp$z)] <- min(mydata$val)


tt <- fld$z
# http://stackoverflow.com/questions/32679844/r-isotherms-as-isolines-using-ggplot2

#filter(dInterp, z<0)
ggplot(data = mydata) +
  geom_tile(data = dInterp, aes(x, y, fill = z), alpha = 0.5, colour = NA) +
  # geom_raster(data = dInterp, aes(x, y, fill = z), alpha = 0.5) + 
  # theme(legend.position = "top") +
  scale_fill_distiller(palette = "Spectral") + #color -- цвет линий
  stat_contour(data = dInterp, aes(x, y, z = z), bins = 4, color="black", size=0.5) +
  geom_point(size = 4, alpha = 1/2, aes(x = lon, y = lat), color = "red") +
  geom_text(aes(lon, lat, label = round(val, digits = 1)), hjust = 0.5, vjust = -1) +
  theme_bw()



mm <- ggmap(fmap, extent = "normal", legend = "topleft") +
  # geom_raster(data = dInterp, aes(x, y, fill = z), alpha = 0.5) + 
  geom_tile(data = dInterp, aes(x, y, fill = z), alpha = 0.5, colour = NA) +
  # theme(legend.position = "top") +
  scale_fill_distiller(palette = "Spectral") + #color -- цвет линий
  stat_contour(data = dInterp, aes(x, y, z = z), bins = 4, color="white", size=0.5) +
  geom_point(data = mydata, size = 4, alpha = 1/2, aes(x = lon, y = lat), color = "red") +
  geom_text(data = mydata, aes(lon, lat, label = round(val, digits = 1)), hjust = 0.5, vjust = -1) +
  theme_bw()

# mm

# а теперь попробуем отобразить растром, понимая все потенциальные проблемы
# проблемы хорошо описаны здесь: https://groups.google.com/forum/embed/#!topic/ggplot2/nqzBX22MeAQ
mm3 <- ggmap(fmap, extent = "normal", legend = "topleft") +
  geom_raster(data = dInterp, aes(x, y, fill = z), alpha = 0.5) +
  coord_cartesian() +
  scale_fill_distiller(palette = "Spectral") + #color -- цвет линий
  stat_contour(data = dInterp, aes(x, y, z = z), bins = 4, color="white", size=0.5) +
  geom_point(data = mydata, size = 4, alpha = 1/2, aes(x = lon, y = lat), color = "red") +
  geom_text(data = mydata, aes(lon, lat, label = round(val, digits = 1)), hjust = 0.5, vjust = -1) +
  theme_bw()


start.time <- Sys.time()
ggsave("plot.png", mm, width = 200, height = 200, units = "mm", dpi = 300)
print(paste0("Вывод длился ", 
             round(as.numeric(difftime(Sys.time(), start.time, unit = "sec")), digits = 0), 
             " сек"))



# ======================
# Google Maps Geocoding API Usage Limits(https://developers.google.com/maps/documentation/geocoding/usage-limits)
# Users of the standard API: 2,500 free requests per day; 10 requests per second 

tile_map <-
  get_map(
    enc2utf8("Москва, Зоологическая 2"),
    language = "ru-RU",
    maptype = "terrain",
    zoom = 16
  )

ggmap(tile_map)

mm2 <- ggmap(tile_map, extent = "normal", legend = "topleft") +
  #geom_tile(data = dInterp, aes(x, y, fill = z), alpha = 0.5) +
  geom_raster(data = dInterp, aes(x, y, fill = z), alpha = 0.5) + 
  coord_cartesian() +
  # scale_fill_distiller(palette = "Spectral") + #color -- цвет линий
  geom_point(data = mydata, size = 4, alpha = 1/2, aes(x = lon, y = lat), color = "red") +
  geom_text(data = mydata, aes(lon, lat, label = round(val, digits = 1)), hjust = 0.5, vjust = -1) +
  theme_bw()


mm2
ggsave("plot2.png", mm2, width = 200, height = 200, units = "mm", dpi = 300)



stop()

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)
downtown <- subset(crime,
                   -95.39681 <= lon & lon <= -95.34188 &
                     29.73631 <= lat & lat <=  29.78400
)

qmplot(lon, lat, data = downtown, maptype = "toner-background", color = I("red"))
stop()
# =============================================
# теперь по новой, берем идеи отсюда: http://stackoverflow.com/questions/24410292/how-to-improve-interp-with-akima

fld <- with(mydata, interp(lon, lat, val,
    xo = seq(min(lon), max(lon), length = 500),
    yo = seq(min(lat), max(lat), length = 500), 
    linear = FALSE, 
    extrap = TRUE
  )) # http://www.statmethods.net/stats/withby.html

filled.contour(x=fld$x, y=fld$y, z=fld$z,
               color.palette=colorRampPalette(c("white", "blue")))

stop()
# =============================================
# http://docs.ggplot2.org/0.9.3.1/stat_contour.html
# Basic plot
v <- ggplot(mydata, aes(x = lon, y = lat, z = val))
# v + stat_contour(bins = 5)
v

qplot(lon, lat, z = val, data = mydata, geom = "contour")

stop()
# =============================================

s <-  interp(mydata$lon, mydata$lat, mydata$val, linear = FALSE, extrap = TRUE) #from akima
# If linear is TRUE (default), linear interpolation is used in the triangles bounded by data points.
# Cubic interpolation is done if linear is set to FALSE. If extrap is FALSE, z-values for points outside
# the convex hull are returned as NA. No extrapolation can be performed for the linear case.
mm <- s$z
# contour(s$x, s$y, s$z)
contour(s)

CL <- contourLines(s$x, s$y, s$z, nlevels = 9)
# nlevels указывает разбиение, но реально нарезок может быть больше (несколько гор)


# http://technocyclist.blogspot.ru/2014/10/plot-contour-polygons-in-leaflet-using-r.html
# Create linestrings
lines <- contourLines(s$x, s$y, s$z, nlevels = 9)
# Create independent polygons within a list
dd1 <- sapply(1:length(lines), function(i) Polygon(as.matrix(cbind(lines[[i]]$x, lines[[i]]$y))))
# Merge all independent polygons into a Polygons object (this contains multiple polygons)
dd2 <- sapply(1:length(lines), function(i) Polygons(list(dd1[[i]]), i))
# Don't forget to remember the contour value for each polygon - we store it into a dataframe for use in the next step
poly_data <- data.frame(Value = sapply(1:length(lines), function(i) lines[[i]]$level))
# Merge the Polygons object dd2 with the dataframe containing the contour level data, poly_data.
dd3 <- SpatialPolygonsDataFrame(SpatialPolygons(dd2), data = poly_data)

# Convert our dd3 SpatialPolygonDataFrame object to JSON
dd_json <- toGeoJSON(dd3, name="MelbourneTree")
# Store the unique levels of the contours, this will come in handy for colouring
values <- unique(sapply(1:length(lines), function(i) lines[[i]]$level))

# Create a style for the Leaflet map
sty <- styleCat(prop="Value", val=values, style.val=brewer.pal(length(values),"Greens"), leg = "Tree Cover")

m <- leaflet() %>% addTiles() %>%
  # setView(lng = 37.58, lat = 55.76, zoom = 14) %>%
  addPolygons(CL[[1]]$x,CL[[1]]$y,fillColor = "red", stroke = FALSE) %>%
  addPolygons(CL[[3]]$x,CL[[3]]$y,fillColor = "green", stroke = FALSE) %>%
  addPolygons(CL[[5]]$x,CL[[5]]$y,fillColor = "blue", stroke = FALSE) %>%
  addPolygons(CL[[7]]$x,CL[[7]]$y,fillColor = "red", stroke = FALSE) %>%
  addPolygons(CL[[9]]$x,CL[[9]]$y,fillColor = "red", stroke = FALSE)

w <- s$z

m

stop()
# =============================================
# http://freakonometrics.hypotheses.org/19473
# http://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package
X <- mydata[c("lat", "lon")]
# bw.ucv -- Bandwidth Selectors for Kernel Density Estimation
kde2d <- bkde2D(X, bandwidth = c(bw.ucv(X[,1]), bw.ucv(X[,2])))
# bkde2d не очень годится, поскольку это density estimate

t <- kde2d$fhat

contour(x=kde2d$x1, y=kde2d$x2, z=kde2d$fhat)
stop()
data(quakes)

