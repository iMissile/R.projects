# разбираемся с обработкой данных с sitewhere
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(tidyr)
library(readr)
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
library(arules)
library(futile.logger)

source("../46 PoC_dashboard/common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

# параметры для доступа к SiteWhere ------------------------------------------------------------------------
# вытащили пока наружу
user_name = 'admin'
user_pass = 'password'
base_url = paste0('http://', user_name, ':', user_pass, '@10.0.0.207:28081/sitewhere/api/')
t_token = 'sitewhere1234567890'


get_tsdf_from_SW_json <- function(data, c_vars) {
  # честный парсинг временных рядов, полученных из выгрузкой из SiteWhere
  # входные данные (data) должны поступать в следующем формате (value - measurementDate по набору метрик)
  # набор колонок\переменных для выборки указывается в виде строкового вектора в переменной c_vars
  # например, так: c('humidity', 'temp', 'pressure')
  # 'data.frame':	3 obs. of  2 variables:
  # $ measurementId: chr  "min" "max" "soil_moisture"
  # $ entries      :List of 3
  # ..$ :'data.frame':	100 obs. of  2 variables:
  #   .. ..$ value          : num  2670304 2670304 2670304 2670304 2670304 ...
  #   .. ..$ measurementDate: chr  "2016-06-06T22:00:06.000+0300" "2016-06-06T22:30:06.000+0300" ...
  # ..$ :'data.frame':	100 obs. of  2 variables:
  #   .. ..$ value          : num  1053200 1053200 1053200 1053200 1053200 ...
  #   .. ..$ measurementDate: chr  "2016-06-06T22:00:06.000+0300" "2016-06-06T22:30:06.000+0300" ...
  # ..$ :'data.frame':	100 obs. of  2 variables:
  #   .. ..$ value          : num  2472880 2476032 2474992 2472880 2472880 ...
  #   .. ..$ measurementDate: chr  "2016-06-06T22:00:06.000+0300" "2016-06-06T22:30:06.000+0300" ...
  
  df0 <- spread(data, measurementId, entries)  # разложили записи по колонкам
  
  # честный парсинг и объединение ----------------------------------------------------------
  # а теперь считаем, что в каждой отдельной записи данные могут быть разных типов: 
  # по мере развития системы идет модификация существующих и добавление новых параметров
  # собираем первый фрейм и к нему подстегиваем шаг за шагом фреймы со последующими параметрами
  data.list <- lapply(c_vars,
                      function(x) {
                        d <- distinct(data.frame(
                          # сразу преобразуем в POSIXct, чтобы не занимать место и ускорить процесс
                          # 2016-05-29T09:28:50.000+0300 --- local time-zone (+3), см. https://www.w3.org/TR/NOTE-datetime
                          timestamp = with_tz(ymd_hms(df0[[x]][[1]]$measurementDate), tz = "Europe/Moscow"), 
                          value = df0[[x]][[1]]$value,
                          stringsAsFactors = FALSE
                        )) # сразу уберем дублирующиеся строки
                        names(d)[2] <- x
                        d
                      })
  
  # , tz = "Europe/Moscow"
  # а теперь объединим все фреймы
  # у нас нет никаких гарантий, что временные метки будут идентичны и совпадать по количеству для различных метрик!
  # например, одну из метрик добавили позже
  
  # соберем вектор времен
  # при объединении POSIXct превращается в numeric, как в общий тип. Связано с возможными разными тайм зонами
  df.time <- data.frame(timestamp = unique(unlist(lapply(data.list, 
                                                         function(x){getElement(x, "timestamp")}))), 
                        stringsAsFactors = FALSE) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin='1970-01-01', tz = "Europe/Moscow"))
  
  df.join <- df.time
  for(i in 1:length(data.list))
  {
    df.join %<>% dplyr::left_join(data.list[[i]], by = "timestamp") #, copy = TRUE)
  }
  
  df.join
}

load_SW_field_data <- function(siteToken, moduleId, assetId) {
  # ------------------------------------------------------------------------
  
  # http://10.0.0.207:28081/sitewhere/api/mt/assets/modules/fs-locations/assets/harry-dirt-pot/assignments/?siteToken=c08a662e-9bbb-4193-a17f-96e0c760e1c3&tenantAuthToken=sitewhere1234567890
  # магическая строка для получения измерений по assignment
  
  # location задан = 'fs-locations', asset тоже задан = 'harry-dirt-pot'.
  # 1. определяем список связей (assignments) на asset-е,
  # 2. выбираем те, которые отвечают датчику влажности
  # 3. достаем временной ряд измерений, соответствующий этому сенсору
  # 4. обогащаем метаинформацией общего характера (location, lon, lat)
  
  url <- paste0(base_url, "mt/assets/modules/", moduleId,
                "/assets/", assetId, 
                "/assignments/?siteToken=", siteToken, 
                "&tenantAuthToken=", t_token)
  resp <- curl_fetch_memory(url)
  write(rawToChar(resp$content), file="./temp/resp.txt")
  data <- fromJSON(rawToChar(resp$content))
  # ищем assignments с sensor_type = soil_moisture

  a <- data$results$assignment
  # error: 'names' attribute [9] must be the same length as the vector [1]
  # http://stackoverflow.com/questions/14153092/meaning-of-ddply-error-names-attribute-9-must-be-the-same-length-as-the-vec
  # убрали вложенные неименованные списки по необязательным полям
  b <- select(a, -state, -metadata) # теперь View(b) работает
  
  # получаем информацию по всем assignments
  d <- data$results$specification
  
  # d1 <- d %>% select(-metadata, -deviceElementSchema)
  # d2 <- d$metadata
  g <- bind_cols(d %>% select(-metadata, -deviceElementSchema), d$metadata)
  
  # соберем интересующую таблицу вручную
  tmp <- data$results
  m <- data.frame(assetId = tmp$assignment$assetId,
                  assignment.token = tmp$assignment$token,
                  deviceId = tmp$device$hardwareId, 
                  specification.token = tmp$device$specificationToken,
                  specification.name = tmp$specification$name,
                  stringsAsFactors = FALSE)
  
  tm1 <- tmp$specification$metadata
  names(tm1) <- paste0("specification.", names(tm1))
  tm2 <- tmp$device$metadata
  names(tm2) <- paste0("device.", names(tm2))
  g <- bind_cols(m, tm1) %>%  bind_cols(tm2)
  
  sensors.df <- g %>%
    filter(specification.sensor_type == 'soil_moisture')
  
  # а теперь запрашиваем сам time-series по обнаруженным assignment
  # http://10.0.0.207:28081/sitewhere/api/assignments/<token>/measurements/series?tenantAuthToken=sitewhere1234567890
  
  str(sensors.df)
  # !!!!!!!!!!!!!!!!!!!!!!! 
  # место для последующей векторизации, пока что ручками принудительно берем только первый элемент из списка
  
  sensor <- head(sensors.df, 1)

  url <- paste0(base_url, "assignments/", sensor$assignment.token, "/measurements/series?tenantAuthToken=", t_token)
  resp <- curl_fetch_memory(url)
  write(rawToChar(resp$content), file="./temp/resp2.txt")
  ts.raw <- fromJSON(rawToChar(resp$content))
  
  # честный парсинг и объединение ----------------------------------------------------------
  df.join <- get_tsdf_from_SW_json(ts.raw, c('soil_moisture', 'min', 'max'))
  
  # обогащаем метаинформацией общего характера (location, lon, lat) ------------------------
  
  # запросим параметры ассета по заданному id. Заберем оттуда location
  url <- paste0(base_url, "assets/categories/", moduleId,
                "/assets/", assetId, 
                "/?siteToken=", siteToken, 
                "&tenantAuthToken=", t_token)
  
  # через curl возникают проблемы с юникодными данными, решил остановиться на более человеко-ориентированном пакете httr
  # resp <- curl_fetch_memory(url)
  # data <- fromJSON(rawToChar(resp$content))
  resp <- GET(url)
  asset <- content(resp)
  
  # df.join$lon <- asset$longitude
  # df.join$lat <- asset$latitude
  # df.join$name <- sensor$deviceId
  # df.join$location <- asset$name
  df.join %<>%
    rename(value = soil_moisture) %>%
    mutate(measurement = value) %>%
    mutate(name = sensor$deviceId) %>%
    mutate(lon = asset$longitude) %>%
    mutate(lat = asset$latitude) %>%
    mutate(type = 'MOISTURE') %>%
    # без as.character возникает ошибка "unsupported type for column 'location' (NILSXP, classes = NULL)"
    mutate(location = as.character(asset$name)) %>% 
    select(-min, -max)
  
  df.join
}

process_SW_field_data <- function(df){
  # на вход получаем data.frame с временным рядом измерений
  # проводим постпроцессинг по масштабированию измерений, их категоризации и пр.

  # 3. частный построцессинг  
  # датчики влажности
  levs <- get_moisture_levels()  
  
  # пересчитываем value для датчиков влажности
  df %<>%
    # пока делаем нормировку к [0, 100] из диапазона 3.3 V грязным хаком
    mutate(value = ifelse(type == 'MOISTURE', measurement / max_moisture_norm(), value)) %>%
    # и вернем для влажности в value редуцированный вольтаж
    # mutate(value  = ifelse(type == 'MOISTURE', measurement / 1000, value))
    # откалибруем всплески
    # кстати, надо подумать, возможно, что после перехода к категоризации, мы можем просто отсекать NA
    mutate(work.status = (type == 'MOISTURE' &
                            value >= head(levs$category, 1) &
                            value <= tail(levs$category, 1)))
  
  # если колонки с категориями не было создано, то мы ее инициализируем
  if(!('level' %in% names(df))) df$level <- NA
  df %<>%
    # считаем для всех, переносим потом только для тех, кого надо
    # превращаем в character, иначе после переноса factor теряется, остаются только целые числа
    mutate(marker = as.character(discretize(value, method = "fixed", categories = levs$category, labels = levs$labels))) %>%
    mutate(level = ifelse(type == 'MOISTURE', marker, level)) %>%
    select(-marker)
  
  df
}

get_SW_field_data <- function(siteToken, moduleId, assetId) {
  df0 <- load_SW_field_data(siteToken, moduleId, assetId) # получаем данные с SiteWhere
  df1 <- process_SW_field_data(df0) # проводим постпроцессинг данных, добавляем вычисляемые поля
  df1
}

# main() --------------------------------------------------

# параметры для запроса TimeSeries ------------------------------------------------------------------------
# на вход получаем siteToken, location, assetId
siteToken = 'c08a662e-9bbb-4193-a17f-96e0c760e1c3'
moduleId = 'fs-locations'
assetId = 'harry-dirt-pot'



github.df <- get_github_field2_data() # считываем прототип того, что хотим получить 
sw.df <- get_SW_field_data(siteToken, moduleId, assetId)

join.df = left_join(github.df, sw.df, by = "timestamp")

# сделаем объединенный data.frame для визуальной сверки результатов



# посмотрим занятые объемы памяти
# http://isomorphism.es/post/92559575719/size-of-each-object-in-rs-workspace
# for (obj in ls()) { message(obj); print(object.size(get(obj)), units='auto') }

mem.df <- data.frame(obj = ls(), stringsAsFactors = FALSE) %>% 
  mutate(size = unlist(lapply(obj, function(x) {object.size(get(x))}))) %>% 
  arrange(desc(size))


## object.size(c(github.df, sw.df, join.df))


stop()



stop()

# забираем историческую погоду с Sitewhere ==================================================================
# write(wh_json, file="./export/wh_json.txt")
getwd()
data <- fromJSON("./data/sitewhere_history.json")

df.join <- get_tsdf_from_SW_json(ts.raw, c('humidity', 'temp', 'pressure'))
df.res <- df.join %>%
  mutate(humidity = round(humidity, 0),
         # temp = round(temp - 273.15, 1), # пересчитываем из кельвинов в градусы цельсия
         pressure = round(pressure * 0.75006375541921, 0) # пересчитываем из гектопаскалей (hPa) в мм рт. столба
  )

object.size(df.res)


stop()

d <- toJSON(list(var1 = 34, var2 = c('rr', 'mm')), pretty = TRUE, auto_unbox = TRUE)

url <- paste0(base_url, "mt/assets/categories/fs-locations/assets/harry-dirt-pot/property/soil_moisture_ts?tenantAuthToken=", 
              t_token)
resp <- curl_fetch_memory(url)




stop()

resp <- curl_fetch_memory("http://admin:password@10.0.0.207:28081/sitewhere/api/assets/categories/fs-locations/assets?tenantAuthToken=sitewhere1234567890")
# wrecs <- rawToChar(resp$content)
data <- fromJSON(rawToChar(resp$content))


# про POST() см. тут: https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html


d <- toJSON(list(var1 = 34, var2 = c('rr', 'mm')), pretty = TRUE, auto_unbox = TRUE)
url <- "http://admin:password@10.0.0.207:28081/sitewhere/api/assets/categories/fs-locations/locations/harry-dirt-pot?tenantAuthToken=sitewhere1234567890"
body <- list(id = "harry-dirt-pot", properties = list(worksite.id = d))

r <- PUT(url, body = body, encode = "json")
stop()

url <- "api.openweathermap.org/data/2.5/"   
MoscowID <- '524901'
APPID <- '19deaa2837b6ae0e41e4a140329a1809'
resp <- GET(paste0(url, "weather?id=", MoscowID, "&APPID=", APPID))
