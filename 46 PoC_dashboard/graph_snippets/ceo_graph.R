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


source("common_funcs.R") # сюда выносим все вычислительные и рисовательные функции


getwd()

# получаем исторические данные по погоде из репозитория Гарика --------------------------------------------------------
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

# head(wh_json)

# получаем прогноз через API --------------------------------------------------------
url <- "api.openweathermap.org/data/2.5/"   
MoscowID <- '524901'
APPID <- '19deaa2837b6ae0e41e4a140329a1809'
# reqstring <- paste0(url, "weather?id=", MoscowID, "&APPID=", APPID)
reqstring <- paste0(url, "forecast?id=", MoscowID, "&APPID=", APPID) 
resp <- GET(reqstring)
if(status_code(resp) == 200){
  r <- content(resp)
}

# получаем погодные данные
m <- r$list
ll <- lapply(m, function(x){ 
  ldate <- getElement(x, 'main')
  ldate$timestamp <- getElement(x, 'dt')
  ldate
  })
l2 <- melt(ll)
# нормализуем под колонки, которые есть в исторических данных
l3 <- tidyr::spread(l2, L2, value) %>% 
  select(-L1, -temp_kf) %>%
  mutate(timestamp = as.integer(timestamp))

# объединяем и вычищаем --------------------------------------------------------

weather.df <- bind_rows(whist.df, l3) %>%
  select(-temp_max, -temp_min, -sea_level, -grnd_level) %>%
  distinct() %>% # удаляем дубли, которые навыдавал API
  mutate(temp = round(temp - 273, 1)) %>% # пересчитываем из кельвинов в градусы цельсия
  mutate(pressure = round(pressure * 0.75006375541921, 0)) %>% # пересчитываем из гектопаскалей (hPa) в мм рт. столба
  mutate(humidity = round(humidity, 0)) %>%
  mutate(timestamp = as.POSIXct(timestamp, origin='1970-01-01')) %>%
  mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) # сделаем почасовую группировку
  
# разметим данные на прошлое и будущее. будем использовать для цветовой группировки
weather.df['time.pos'] <- ifelse(weather.df$timestamp < now(), "PAST", "FUTURE")

# причешем данные для графика у Паши + проведем усреднение по часовым группам
outw.df <- weather.df %>%
  filter(timegroup >= floor_date(now() - days(7), unit = "day")) %>%
  filter(timegroup <= ceiling_date(now() + days(3), unit = "day")) %>%
  group_by(timegroup, time.pos) %>%
  summarise(temp = mean(temp), pressure = mean(pressure), humidity = mean(humidity)) %>%
  ungroup

# чтобы график не был разорванным, надо продублировать максимальную точку из PAST в группу FUTURE
POI.df <- outw.df %>%
  filter(time.pos == 'PAST') %>%
  filter(timegroup == max(timegroup)) %>%
  mutate(time.pos = 'FUTURE')

outw.df <- outw.df %>%
  bind_rows(POI.df) %>%
  arrange(timegroup)


# сделаем выгрузку в json --------------------------------------------------------
# df2 <- data.frame(timestamp = round(as.numeric(outw.df$timegroup), 0), 
#                   air_temp_past = ifelse(outw.df$time.pos == "PAST", round(outw.df$temp, 1), NA),
#                   air_temp_future = ifelse(outw.df$time.pos == "FUTURE", round(outw.df$temp, 1), NA),
#                   air_humidity_past = ifelse(outw.df$time.pos == "PAST", round(outw.df$humidity, 1), NA),
#                   air_humidity_future = ifelse(outw.df$time.pos == "FUTURE", round(outw.df$humidity, 1), NA)) %>%
#   arrange(timestamp)

df3 <- with(outw.df, {
  data.frame(timestamp = round(as.numeric(timegroup), 0), 
                  air_temp_past = ifelse(time.pos == "PAST", round(temp, 1), NA),
                  air_temp_future = ifelse(time.pos == "FUTURE", round(temp, 1), NA),
                  air_humidity_past = ifelse(time.pos == "PAST", round(humidity, 1), NA),
                  air_humidity_future = ifelse(time.pos == "FUTURE", round(humidity, 1), NA)) %>%
  arrange(timestamp)
})

x <- jsonlite::toJSON(list(results = df3), pretty = TRUE)
write(x, file="./export/real_weather.json")

# отобразим для себя --------------------------------------------------------

# https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
p1 <- ggplot(outw.df, aes(timegroup, temp, colour = time.pos)) +
  # ggtitle("График температуры") +
  # scale_fill_brewer(palette="Set1") +
  scale_fill_brewer(palette = "Paired") +
  # geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
  # geom_point(shape = 1, size = 3) +
  # geom_line(lwd = 1, linetype = 'dashed', color = "red") +
  scale_x_datetime(breaks = date_breaks("1 days"), minor_breaks = date_breaks("6 hours")) +
  geom_line(lwd = 1.2) +
  # theme_igray() +
  theme(legend.position="none")

p1
