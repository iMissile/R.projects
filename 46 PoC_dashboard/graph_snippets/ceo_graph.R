rm(list=ls()) # очистим все переменные

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


# https://cran.r-project.org/web/packages/curl/vignettes/intro.html
req <- curl_fetch_memory("https://raw.githubusercontent.com/iot-rus/Moscow-Lab/master/weather.txt")
wrecs <- rawToChar(req$content) # weather history
# wh_json <- gsub('\\\"', "'", txt, perl = TRUE) 
# заменим концы строк на , и добавим шапочку и окончание для формирования семантически правильного json
# последнюю ',' надо удалить, может такое встретиться (перевод строки)
tmp <- paste0('{"res":[', gsub("\\n", ",\n", wrecs, perl = TRUE), ']}')
wh_json <- gsub("},\n]}", "}]}", tmp)
# t <- cat(wh_json)
write(wh_json, file="./export/wh_json.txt")
data <- fromJSON(wh_json)

whist.df <- data$res$main
whist.df$timestamp <- data$res$dt
  
# t0 <- '{"coord":{"lon":37.61,"lat":55.76},"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"base":"cmc stations","main":{"temp":291.77,"pressure":1012,"humidity":72,"temp_min":290.15,"temp_max":295.35},"wind":{"speed":4,"deg":340},"clouds":{"all":0},"dt":1464008912,"sys":{"type":1,"id":7323,"message":0.0031,"country":"RU","sunrise":1463965411,"sunset":1464025820},"id":524894,"name":"Moskva","cod":200}'
# t1 <- '{"coord":{"lon":37.61,"lat":55.76},"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"base":"stations","main":{"temp":291.01,"pressure":1012,"humidity":72,"temp_min":289.15,"temp_max":292.15},"visibility":10000,"wind":{"speed":4,"deg":330},"clouds":{"all":0},"dt":1464007798,"sys":{"type":1,"id":7323,"message":0.0354,"country":"RU","sunrise":1463965412,"sunset":1464025819},"id":524894,"name":"Moskva","cod":200}'
# t <- paste0('{"results":[', t0, ',', t1, ']}')
# mdata <- fromJSON(t)

head(wh_json)

stop()

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
  
