# library(influxdbr)
#library(curl)
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

# get Time-Series data
getTSdata <- function(depth = "1d") {
  # depth -- глубина просмотра от текущей даты: 1 сутки (1d); 1 неделя (1w)
  parameterID <- "Dg3i5sJyPsXse53O"
  currtime <-
    floor(as.numeric(Sys.time()) * 1000) # http://currentmillis.com/
  
  # снимаем исторические данные грязным хаком
  url <- "cloud.iot-playground.com:8086"
  query <-
    paste0(
      "/query?q=select+time%2C+Value+from+hist+where+Id+%3D+%27",
      parameterID,
      "%27+and+time+%3E+now()+-+",
      depth,
      "&db=eiotclouddb&epoch=ms&_=",
      currtime
    )
  # делаем выборку строго за предыдущие сутки
  # query <- paste0("/query?q=select+time%2C+Value+from+hist+where+Id+%3D+%27", parameterID, "%27+and+time+%3E+now()+-+2d+and+time+%3C+now()+-+1d&db=eiotclouddb&epoch=ms&_=", currtime)
  resp <-
    GET(paste0(url, query),
        add_headers("Authorization" = "Basic ZWFzeWlvdDplYXN5aW90"))
  cont <- content(resp)
  
  # cont <- fromJSON('{"results":[{"series":[{"name":"hist","columns":["time","Value"],"values":[[1456842993897,1000],[1456843135556,1000],[1456843148417,1000],[1456843536525,2.667e+06],[1456843542450,2.67e+06],[1456843548310,2.672e+06],[1456843554039,2.674e+06]]}]}]}')
  
  # достаем список значений {time, Value}
  raw_measure <- cont$results[[1]]$series[[1]]$values
  
  m_df <- raw_measure %>%
    lapply(function(x) data.frame(raw_time = x[[1]], value = x[[2]])) %>%
    bind_rows() %>%
    mutate(timestamp = as.POSIXct(raw_time/1000, origin = "1970-01-01", tz = "GMT") )
}

getTokenID <- function() {
  # Get Token
  url <- "http://cloud.iot-playground.com:40404/RestApi/v1.0/Token/List"
  instanceID <- "56d57092c943a05b64ba2682"
  resp <- GET(url, add_headers("Eiot-Instance" = instanceID))
  tokenID <- content(resp)[[1]]$Token  # Advanced R, 3.2 Subsetting operators
  tokenID
}

getCurrentCraftPos <- function(tokenID)
{
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
  point
}
# =================================================
date_format_tz <- function(format = "%Y-%m-%d", tz = "UTC") {
  function(x) format(x, format, tz=tz)
}

