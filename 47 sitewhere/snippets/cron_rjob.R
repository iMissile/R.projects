rm(list=ls()) # очистим все переменные

library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(readr)
library(reshape2)
library(curl)
library(httr)
library(jsonlite)
library(magrittr)
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


# запрос и формирование данных по осадкам (прошлое и прогноз) =====================================
weather.df <- prepare_raw_weather_data()

# d <- dmy_hm("23-12-2015 4:19")
# str(date(dmy_hm("23-12-2015 4:19")))
dfw2 <- calc_rain_per_date(weather.df)

flog.info("Rain history & forecast")
flog.info(capture.output(print(dfw2)))

# сгенерируем json с погодой под требования Паши ---------------------------------------------------
# http://arxiv.org/pdf/1403.2805v1.pdf  |   http://arxiv.org/abs/1403.2805
x <- jsonlite::toJSON(list(results = dfw2), pretty = TRUE)
write(x, file = rain_filename)

# запрос и формирование данных по погоде ==========================================================
df <- get_weather_df(back_days = 7, forward_days = 3)

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

# запрос и формирование данных по данным сенсоров ==============================================
raw.df <- load_github_field_data()
# if (!is.na(df)) { raw.df <- df}
#     name      lat      lon value work.status           timestamp   location
#    (chr)    (dbl)    (dbl) (dbl)       (lgl)              (time)      (chr)
browser()

# формирование временного ряда по сенсорам ---------------------------------------------------
# проведем усреднение по временным группам, если измерения проводились несколько раз в течение этого времени
# усредняем только по рабочим датчикам

# фильтруем 7 дней назад, 3 вперед, почасовая  группировка
# сгруппируем по временным интервалам
avg.df <- raw.df %>%
  mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) %>%
  filter(timegroup >= floor_date(now() - days(7), unit = "day")) %>%
  filter(timegroup <= ceiling_date(now() + days(3), unit = "day")) %>%  
  filter(work.status) %>%
  group_by(location, timegroup) %>%
  summarise(value.mean = round(mean(value), 0), 
            value.min = round(min(value), 0), 
            value.max = round(max(value), 0)) %>%
  mutate(timestamp.human = timegroup) %>%
  mutate(timestamp = round(as.numeric(timegroup), 0)) %>%
  ungroup() # очистили группировки

flog.info("Time-series data")
flog.info(capture.output(head(avg.df, n = 4)))

x <- jsonlite::toJSON(list(results = avg.df), pretty = TRUE)
write(x, file = sensorts_filename)

# формирование временного среза в пространстве по сенсорам ---------------------------------------------------
sensors.df <- prepare_sesnors_mapdf(raw.df, slicetime = lubridate::now()) %>%
  mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) %>%
  mutate(timestamp.human = timestamp) %>%
  mutate(timestamp = round(as.numeric(timegroup), 0))

flog.info("Time-slice data")
flog.info(capture.output(head(sensors.df, n = 4)))

x <- jsonlite::toJSON(list(results = sensors.df), pretty = TRUE)
write(x, file = sensorslice_filename)

flog.info("Job finished")
