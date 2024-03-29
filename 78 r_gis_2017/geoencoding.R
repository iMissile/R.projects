# library(ggmap)
library(tmap)
library(tmaptools) #geocode

# map <- get_map(location = 'uk', zoom = 8, source = "osm") # ������!!!

data(Europe)
# qtm(Europe)

# ----
# eiffelTower <- geocode_OSM("Eiffel Tower", as.SPDF=TRUE)
city_geocode <- geocode_OSM("������,+��������+�����,+���+7", details=TRUE, as.SPDF=TRUE)
city_geocode <- purrr::safely(geocode_OSM, otherwise=NULL)("����,+��������+�����,+���+7", details=TRUE, as.SPDF=TRUE)
city_geocode <- try(geocode_OSM("����,+��������+�����,+���+7", details=TRUE, as.SPDF=TRUE))
# city_geocode <- geocode_OSM("����,+��������+�����,+���+7", details=TRUE, as.data.frame=TRUE)
sp::coordinates(city_geocode) <- ~lon+lat

data(World)
tm_shape(World, bbox="Russia")

tm_shape(city_geocode) +
  tm_dots(col = "red")

