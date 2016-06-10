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


get_tsdf_from_SW_json <- function(data, c_vars) {
  # честный парсинг временных рядов, полученных из выгрузкой из SiteWhere
  # входные данные (data) должны поступать в следующем формате (value - measurementDate по набору метрик)
  # наборк колонок\переменных для выборки указывается в виде строкового вектора в переменной c_vars
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

# ------------------------------------------------------------------------

user_name = 'admin'
user_pass = 'password'
base_url = paste0('http://', user_name, ':', user_pass, '@10.0.0.207:28081/sitewhere/api/')
t_token = 'sitewhere1234567890'


# http://10.0.0.207:28081/sitewhere/api/mt/assets/modules/fs-locations/assets/harry-dirt-pot/assignments/?siteToken=c08a662e-9bbb-4193-a17f-96e0c760e1c3&tenantAuthToken=sitewhere1234567890
# магическая строка для получения измерений по assignment

url <- paste0(base_url, "mt/assets/modules/fs-locations/assets/harry-dirt-pot/assignments/?siteToken=c08a662e-9bbb-4193-a17f-96e0c760e1c3&tenantAuthToken=", 
              t_token)
req <- curl_fetch_memory(url)
write(rawToChar(req$content), file="./temp/resp.txt")
data <- fromJSON(rawToChar(req$content))
# ищем assignments с sensor_type = soil_moisture


a <- data$results$assignment
# error: 'names' attribute [9] must be the same length as the vector [1]
# http://stackoverflow.com/questions/14153092/meaning-of-ddply-error-names-attribute-9-must-be-the-same-length-as-the-vec
b <- select(a, -state, - metadata) # убрали вложенные неименованные списки по необязательным полям
# теперь View(b) работает

# получаем информацию по всем assignments
d <- data$results$specification


d1 <- d %>% select(-metadata, -deviceElementSchema)
d2 <- d$metadata
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

sensors <- g %>%
  filter(specification.sensor_type == 'soil_moisture')

# а теперь запрашиваем сам time-series по обнаруженным assignment
# http://10.0.0.207:28081/sitewhere/api/assignments/<token>/measurements/series?tenantAuthToken=sitewhere1234567890
token <- head(sensors$assignment.token, 1)

url <- paste0(base_url, "assignments/", token, "/measurements/series?tenantAuthToken=", t_token)
req <- curl_fetch_memory(url)
write(rawToChar(req$content), file="./temp/resp2.txt")
ts.raw <- fromJSON(rawToChar(req$content))

# честный парсинг и объединение ----------------------------------------------------------
df.join <- get_tsdf_from_SW_json(ts.raw, c('soil_moisture', 'min', 'max'))

object.size(df.join)

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

req <- curl_fetch_memory("http://admin:password@10.0.0.207:28081/sitewhere/api/assets/categories/fs-locations/assets?tenantAuthToken=sitewhere1234567890")
# wrecs <- rawToChar(req$content)
data <- fromJSON(rawToChar(req$content))


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
