# library(ggmap)
library(tmap)
library(tmaptools) #geocode

# map <- get_map(location = 'uk', zoom = 8, source = "osm") # ошибка!!!

data(Europe)
# qtm(Europe)

# ----
# eiffelTower <- geocode_OSM("Eiffel Tower", as.SPDF=TRUE)
city_geocode <- geocode_OSM("Москва,+Тверская+улица,+дом+7", details=TRUE, as.SPDF=TRUE)
city_geocode <- purrr::safely(geocode_OSM, otherwise=NULL)("Марс,+Тверская+улица,+дом+7", details=TRUE, as.SPDF=TRUE)
city_geocode <- try(geocode_OSM("Марс,+Тверская+улица,+дом+7", details=TRUE, as.SPDF=TRUE))
# city_geocode <- geocode_OSM("Марс,+Тверская+улица,+дом+7", details=TRUE, as.data.frame=TRUE)
sp::coordinates(city_geocode) <- ~lon+lat

data(World)
tm_shape(World, bbox="Russia")

tm_shape(city_geocode) +
  tm_dots(col = "red")

