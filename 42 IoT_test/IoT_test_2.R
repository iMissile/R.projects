library(influxdbr)
library(curl)
library(jsonlite)
library(dplyr)
library(magrittr)
library(httr)
library(ggplot2)
library(scales)

# Fetching JSON data from REST APIs  (https://cran.r-project.org/web/packages/jsonlite/vignettes/json-apis.html)
# URL: http://cloud.iot-playground.com:40404/RestApi/v1.0/Parameter/[id]/Value

# username - pkochnov
# pass Qwerty123
# instanceId = 56d57092c943a05b64ba2682

parameterID <- "Dg3i5sJyPsXse53O"
currtime <- floor(as.numeric(Sys.time()) * 1000) # http://currentmillis.com/

# снимаем исторические данные гр€зным хаком
url <- "cloud.iot-playground.com:8086"
query <- paste0("/query?q=select+time%2C+Value+from+hist+where+Id+%3D+%27", parameterID, "%27+and+time+%3E+now()+-+1d&db=eiotclouddb&epoch=ms&_=", currtime)
# делаем выборку строго за предыдущие сутки
# query <- paste0("/query?q=select+time%2C+Value+from+hist+where+Id+%3D+%27", parameterID, "%27+and+time+%3E+now()+-+2d+and+time+%3C+now()+-+1d&db=eiotclouddb&epoch=ms&_=", currtime)
resp <- GET(paste0(url, query), add_headers("Authorization" = "Basic ZWFzeWlvdDplYXN5aW90"))
cont <- content(resp)

# cont <- fromJSON('{"results":[{"series":[{"name":"hist","columns":["time","Value"],"values":[[1456842993897,1000],[1456843135556,1000],[1456843148417,1000],[1456843536525,2.667e+06],[1456843542450,2.67e+06],[1456843548310,2.672e+06],[1456843554039,2.674e+06]]}]}]}')

# достаем список значений {time, Value}
raw_measure <- cont$results[[1]]$series[[1]]$values
m_df <- raw_measure %>%
  lapply(function(x) data.frame(raw_time = x[[1]], value = x[[2]])) %>%
  bind_rows() %>%
  mutate(timestamp = as.POSIXct(raw_time/1000, origin = "1970-01-01", tz = "GMT") )
  
# =================================================
date_format_tz <- function(format = "%Y-%m-%d", tz = "UTC") {
  function(x) format(x, format, tz=tz)
}

gp <- ggplot(data = m_df, aes(x=timestamp, y=value)) +
  theme_bw() +
  geom_point(size = 2, fill = "white", shape = 21, na.rm = TRUE) +    # White fill
  geom_line(size = 0.5, color = "blue", na.rm = TRUE) +
  # scale_color_manual(values = wes_palette("Moonrise2")) +
  # дельта в 3 часа при выводе на суточном графике
  # http://stackoverflow.com/questions/10339618/what-is-the-appropriate-timezone-argument-syntax-for-scale-datetime-in-ggplot
  #    scale_x_datetime(labels = date_format_tz("%H:%M", tz="Europe/Moscow"), breaks = date_breaks("1 hours"), minor_breaks = date_breaks("30 mins")) +
  scale_x_datetime(labels = date_format_tz("%d.%m %H:%M", tz="Europe/Moscow"), breaks = date_breaks("4 hours"), minor_breaks = date_breaks("30 mins")) +
  labs(x="ƒата", y="Interface load, %")
# %d.%m (%a)  
gp

