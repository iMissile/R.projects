# library(influxdbr)
#library(curl)
library(jsonlite)
library(dplyr)
library(magrittr)
library(httr)
library(ggplot2)
library(scales)
library(leaflet)

source("IoT_funcs.R")
# Fetching JSON data from REST APIs  (https://cran.r-project.org/web/packages/jsonlite/vignettes/json-apis.html)
# URL: http://cloud.iot-playground.com:40404/RestApi/v1.0/Parameter/[id]/Value

# username - pkochnov
# pass Qwerty123
# instanceId = 56d57092c943a05b64ba2682


# ================================================================
# получаем координаты самолета
# Sensor.Latitude, ID RkBJ8qQhRzgxv763
# Sensor.Longitude ID tAoFkzPlZWiUNzro
# карта: https://maps.yandex.ru/?text=51.27798,6.75819

# Get Token
url <- "http://cloud.iot-playground.com:40404/RestApi/v1.0/Token/List"
instanceID <- "56d57092c943a05b64ba2682"
resp <- GET(url, add_headers("Eiot-Instance" = instanceID))
tokenID <- content(resp)[[1]]$Token  # Advanced R, 3.2 Subsetting operators

# Get parameter value
point <-
  lapply(list("RkBJ8qQhRzgxv763", "tAoFkzPlZWiUNzro"),
       function(id) {
         paste0("http://cloud.iot-playground.com:40404/RestApi/v1.0/Parameter/",
                id,
                "/Value") %>%
           GET(add_headers("EIOT-AuthToken" = tokenID)) %>%
           content() %>%
           .[['Value']] %>%
           as.numeric()
       })

# Visualising your hiking trails and photos with My Tracks, R and Leaflet
# http://mhermans.net/hiking-gpx-r-leaflet.html

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = point[[2]], lat = point[[1]], popup="Борт")
  # addMarkers(lng = 28.5645, lat = 56.3409, popup="Борт")
m  # Print the map

# 56.3409/28.5645

stop()
# ================================================================
m_df <- getTSdata("1d") # get Time-Series data as data frame

gp <- ggplot(data = m_df, aes(x=timestamp, y=value)) +
  theme_bw() +
  geom_point(size = 2, fill = "white", shape = 21, na.rm = TRUE) +    # White fill
  geom_line(size = 0.5, color = "blue", na.rm = TRUE) +
  # scale_color_manual(values = wes_palette("Moonrise2")) +
  # дельта в 3 часа при выводе на суточном графике
  # http://stackoverflow.com/questions/10339618/what-is-the-appropriate-timezone-argument-syntax-for-scale-datetime-in-ggplot
  #    scale_x_datetime(labels = date_format_tz("%H:%M", tz="Europe/Moscow"), breaks = date_breaks("1 hours"), minor_breaks = date_breaks("30 mins")) +
  # scale_x_datetime(labels = date_format_tz("%d.%m %H:%M", tz="Europe/Moscow"), breaks = date_breaks("4 hours"), minor_breaks = date_breaks("30 mins")) +
  scale_x_datetime(labels = date_format_tz("%H", tz="Europe/Moscow"), breaks = date_breaks("1 hours"), minor_breaks = date_breaks("30 mins")) +
  labs(x="Дата", y="Interface load, %")
# %d.%m (%a)  
gp

