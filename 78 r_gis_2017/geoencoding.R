library(ggmap)
library(tmap)

# map <- get_map(location = 'uk', zoom = 8, source = "osm") # ошибка!!!

data(Europe)
qtm(Europe)
