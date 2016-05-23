library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(jsonlite)
library(magrittr)
library(curl)
library(httr)
library(ggthemes)
library(ggdendro) # для пустой темы
#library(ggmap)
library(RColorBrewer) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
library(scales)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()


url <- "api.openweathermap.org/data/2.5/"   
MoscowID <- '524901'
APPID <- '19deaa2837b6ae0e41e4a140329a1809'
# reqstring <- paste0(url, "weather?id=", MoscowID, "&APPID=", APPID)
reqstring <- paste0(url, "forecast?id=", MoscowID, "&APPID=", APPID) 
resp <- GET(reqstring)
if(status_code(resp) == 200){
  r <- content(resp)
  # конструируем вектор
  # d <- data.frame(
  #   # timestamp = now(),
  #   timestamp = as.POSIXct(r$dt, origin='1970-01-01'),
  #   temp = round(r$main$temp - 273, 1), # пересчитываем из кельвинов в градусы цельсия
  #   pressure = round(r$main$pressure * 0.75006375541921, 0), # пересчитываем из гектопаскалей (hPa) в мм рт. столба
  #   humidity = round(r$main$humidity, 0)
    # precipitation = r$main$precipitation
  #)
}

# получаем погодные данные
m <- r$list
ll <- lapply(m, function(x){ 
  ldate <- getElement(x, 'main')
  ldate$timestamp <- getElement(x, 'dt')
  ldate
  })
l2 <- melt(ll)
l3 <- tidyr::spread(l2, L2, value) %>% 
  select(-temp_kf, -temp_max, -temp_min, -sea_level, -grnd_level, -L1) %>%
  mutate(temp = round(temp - 273, 1)) %>% # пересчитываем из кельвинов в градусы цельсия
  mutate(pressure = round(pressure * 0.75006375541921, 0)) %>% # пересчитываем из гектопаскалей (hPa) в мм рт. столба
  mutate(humidity = round(humidity, 0)) %>%
  mutate(timestamp = as.POSIXct(timestamp, origin='1970-01-01'))
  
