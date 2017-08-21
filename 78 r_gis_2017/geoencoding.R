library(ggmap)
library(tmap)
library(tmaptools) #geocode

# map <- get_map(location = 'uk', zoom = 8, source = "osm") # ошибка!!!

data(Europe)
qtm(Europe)


