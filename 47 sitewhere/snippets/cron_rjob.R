rm(list=ls()) # очистим все переменные

library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
library(curl)
library(httr)
library(jsonlite)
library(magrittr)
library(arules)
library(futile.logger)

# if (getwd() == "/home/iot-rus") {
#   # запущен на сервере
#   logfilename <- "/home/iot-rus/Rjobs/log/iot.log"
#   weatherfilename <- "/home/iot-rus/Rjobs/output/real_weather.json"
#   sensorfilename <- "/home/iot-rus/Rjobs/output/real_sensor.json"
# } else {
#   logfilename <- "./log/iot.log"
#   weatherfilename <- "./export/real_weather.json"
#   sensorfilename <- "./export/real_sensor.json"
# }

# запуск скрипта из рабочей директории, так настраиваем cron
log_filename <- "./log/iot.log"
weather_filename <- "./output/real_weather.json"
sensorts_filename <- "./output/real_sensor_ts.json"
sensorslice_filename <- "./output/real_sensor_slice.json"
rain_filename <- "./output/rain.json"

# инициализация ----------------------------------------------
flog.appender(appender.file(log_filename))
flog.threshold(TRACE)
flog.info("Job started")
flog.info("Working directory: %s", getwd())
flog.info("Processing started")

source("../46 PoC_dashboard/common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

# параметры для подключения к SiteWhere
user_name = 'admin'
user_pass = 'password'
base_url = paste0('http://', user_name, ':', user_pass, '@10.0.0.207:28081/sitewhere/api/')
t_token = 'sitewhere1234567890'


if(TRUE){

# запрос и формирование данных по осадкам (прошлое и прогноз) =====================================
raw_weather.df <- prepare_raw_weather_data()

# d <- dmy_hm("23-12-2015 4:19")
# str(date(dmy_hm("23-12-2015 4:19")))
dfw2 <- calc_rain_per_date(raw_weather.df)

flog.info("Rain history & forecast")
flog.info(capture.output(print(dfw2)))

# сгенерируем json с погодой под требования Паши ---------------------------------------------------
# http://arxiv.org/pdf/1403.2805v1.pdf  |   http://arxiv.org/abs/1403.2805
x <- jsonlite::toJSON(list(results = dfw2), pretty = TRUE)
write(x, file = rain_filename)

# запрос и формирование данных по погоде ==========================================================
df <- get_weather_df(raw_weather.df, back_days = 7, forward_days = 3)

# http://stackoverflow.com/questions/25550711/convert-data-frame-to-json
df3 <- with(df, {
  data.frame(timestamp = round(as.numeric(timegroup), 0), 
             timestamp.human = timegroup,
             rain3h_av = rain3h_av,
             air_temp_past = ifelse(time.pos == "PAST", round(temp, 0), NA),
             air_temp_future = ifelse(time.pos == "FUTURE", round(temp, 0), NA),
             air_humidity_past = ifelse(time.pos == "PAST", round(humidity, 0), NA),
             air_humidity_future = ifelse(time.pos == "FUTURE", round(humidity, 0), NA)) %>%
    arrange(timestamp)
})
flog.info("Weather data")
flog.info(capture.output(summary(df3)))

# сгенерируем json с погодой под требования Паши ---------------------------------------------------
# http://arxiv.org/pdf/1403.2805v1.pdf  |   http://arxiv.org/abs/1403.2805
x <- jsonlite::toJSON(list(results = df3), pretty = TRUE)
write(x, file = weather_filename)

}
# запрос и формирование данных по данным сенсоров ==============================================
# raw.df <- load_github_field_data()
# if (!is.na(df)) { raw.df <- df}
#     name      lat      lon value work.status           timestamp   location
#    (chr)    (dbl)    (dbl) (dbl)       (lgl)              (time)      (chr)

raw.df <- load_github_field2_data()
# формирование временного ряда по сенсорам ---------------------------------------------------
# проведем усреднение по временным группам, если измерения проводились несколько раз в течение этого времени
# усредняем только по рабочим датчикам

# фильтруем 7 дней назад, 3 вперед, почасовая  группировка
# сгруппируем по временным интервалам
avg.df <- raw.df %>%
  filter(type == 'MOISTURE') %>% # выгружаем только влажность
  mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) %>%
  filter(timegroup >= floor_date(now() - days(7), unit = "day")) %>%
  filter(timegroup <= ceiling_date(now() + days(3), unit = "day")) %>%  
  filter(work.status) %>%
  # нормируем микровольты в значения
  mutate(value = value/3300) %>%
  group_by(location, timegroup) %>%
  summarise(value.min = round(min(value), 3), 
            value.mean = round(mean(value), 3), 
            value.max = round(max(value), 3)) %>%
  mutate(timestamp.human = timegroup) %>%
  mutate(timestamp = round(as.numeric(timegroup), 0)) %>%
  ungroup() # очистили группировки
  
  # убираем доп. мусор по просьбе Паши
avg.df %<>% select(-location, -timestamp.human)

flog.info("Time-series data: file")
flog.info(capture.output(head(avg.df, n = 4)))

# подготовим диапазоны
drange <- c(2210, 2270, 2330, 2390, 2450, 2510)/3300 # нормировка на милливольты
# чтобы получить по строчкам, я собрал не в список, а в data.frame
levs <- data.frame(low = head(drange, -1), up = tail(drange, -1), 
             name = c('WET+', 'WET', 'NORM', 'DRY', 'DRY+'))

jdata <- jsonlite::toJSON(list(soil_moisture = list(levels = levs, ts = avg.df)), pretty = TRUE)
write(jdata, file = sensorts_filename)

# а теперь положим на sitewhere
url <- paste0(base_url, "mt/assets/categories/fs-locations/assets/harry-dirt-pot/property/dashboard?tenantAuthToken=", 
              t_token)
resp <- PUT(url, body = jdata, encode = "json")
flog.info(paste0("Time-series data: SiteWhere PUT response code = ", resp$status_code))


# формирование временного среза в пространстве по сенсорам ---------------------------------------------------
sensors.df <- prepare_sensors_mapdf(raw.df, slicetime = lubridate::now()) %>%
  mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) %>%
  mutate(timestamp.human = timestamp) %>%
  mutate(timestamp = round(as.numeric(timegroup), 0))

flog.info("Time-slice data")
flog.info(capture.output(head(sensors.df, n = 4)))

x <- jsonlite::toJSON(list(results = sensors.df), pretty = TRUE)
write(x, file = sensorslice_filename)

flog.info("Job finished")
