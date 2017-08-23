rm(list=ls()) # очистим все переменные
library(tidyverse)
library(readxl)
library(stringi)
library(httr)
library(crul)
library(jsonlite)
library(tmaptools) #geocode_OSM
library(pryr)
# library(reshape2)
# library(rjson)
# library(XML)
# library(foreach)
# library(debug)
# library(doParallel)
library(tictoc)
library(futile.logger)

log_name <- "app.log"

flog.appender(appender.tee(log_name))
flog.threshold(TRACE)
flog.info("App started")

# считываем таблицу городов
cities_df <- read_excel("./data/города.xlsx") %>%
  select(city_name=`Город (рус)`) %>%
  head(5) %>%
  # add_row(city_name="Арзамас", .before=1) %>%
  # add_row(city_name="Незнаемо что", .before=1) %>%
  distinct()
  
# Геокодеры -------
geoOSM <- function(location){
  #' количество запросов в сутки - до 25 000 
  # превращаем location строку с разделителями в набор лексем, соединенных `+`
  # browser()
  loc <- location %>% 
    stri_replace_all_regex(pattern=c("(\\s+)", ","),
                           replacement=c("+", ""), 
                           vectorize_all=FALSE)
  r <- purrr::safely(geocode_OSM)(loc, details=TRUE, as.SPDF=TRUE)$result
  # browser()
  flog.info(paste0("OSM Geo_API response: request=", location, ", error=", is.null(r)))

  res <- tibble(location_src=location,
                api="geoOSM",
                formatted_address=r$display_name,
                lat=r$lat,
                lon=r$lon)
}

geoYandex <- function(location){
  #' количество запросов в сутки - до 25 000 
  # превращаем location строку с разделителями в набор лексем, соединенных `+`
  loc <- location %>% 
    stri_replace_all_regex(pattern=c("(\\s+)", ","),
                           replacement=c("+", ""), 
                           vectorize_all=FALSE)
  ## взято из документации crul, стр 11
  # query params are URL encoded for you, so DO NOT do it yourself
  ## if you url encode yourself, it gets double encoded, and that's bad
  base_url <- "http://geocode-maps.yandex.ru/1.x/?geocode="
  geo_url <- paste0(base_url, loc, "&format=json")
  # r <- fromJSON(content(httr::GET(geo_url, verbose()), as='text'))
  r <- fromJSON(content(httr::GET(geo_url), as='text'))
  # в ответе яндекс выдает кроме основного объекта и похожие. 
  # например, на Абакан он даст и город и аэропорт и реку

  # проанализируем метаданные, чтобы понять, насколько точный запрос
  # https://tech.yandex.ru/maps/doc/geocoder/desc/reference/GeocoderResponseMetaData-docpage/
  meta <- r$response$GeoObjectCollection$metaDataProperty$GeocoderResponseMetaData
  flog.info(paste0("Yandex Geo_API response: request=", meta$request, ", found=", meta$found))
  
  # заберем первый элемент из найденных, просто отбрасывать плохо
  # например по поиску "город Арзамас" в ответ приходит и "Саров"
  obj <- r$response$GeoObjectCollection$featureMember %>% head(1)
  # достаем (lon, lat)
  pos <- obj$GeoObject$Point$pos %>% 
    stri_split_regex(pattern="\\s+") %>%
    flatten_chr() %>%
    as.numeric()
  
  # browser()
  res <- tibble(location_src=location,
                api="geoYandex",
                formatted_address=obj$GeoObject$metaDataProperty$GeocoderMetaData$Address$formatted,
                lat=pos[[2]],
                lon=pos[[1]])
}

geoGoogle <- function(location){
  # превращаем location строку с разделителями в набор лексем, соединенных `+`
  loc <- location %>% 
    stri_replace_all_regex(pattern=c("(\\s+)", ","), 
                           replacement=c("+", ""), 
                           vectorize_all=FALSE)

  base_url <- "http://maps.googleapis.com/maps/api/geocode/json?address="
  geo_url <- paste0(base_url, loc, "&language=ru")
  r <- fromJSON(content(httr::GET(geo_url), as='text'))
  flog.info(paste0("Google Geo_API response: request=", location, ", status=", r$status))

  # надо проверить, что status="OK". Если ответ пустой, то status="ZERO_RESULTS"
  # browser()
  res <- tibble(location_src=location,
                api="geoGoogle",
                formatted_address=r$results$formatted_address,
                lat=r$results$geometry$location$lat,
                lon=r$results$geometry$location$lng)
}

# Проверка ----------------
# l <- addr %>% purrr::map(geoYandex, IsAddressFilter=T)

processGeoEncoding <- function(fun, city_name) {
  # browser()
  res <- city_name %>%
    {stri_join("город ", .)} %>% 
    # purrr::map(safely(geoGoogle)) %>%
    # purrr::map(safely(geoYandex)) %>%
    # purrr::map(safely(geoOSM)) %>%
    purrr::map(safely(fun)) %>% 
    transpose()
  # выгребем только не NULL результаты и разложим их 
  ok <- res$error %>% map_lgl(is_null)
  df2 <- res$result %>% 
    keep(ok) %>% 
    {tibble(val=.)} %>% 
    unnest() %>%
    # отрежем назад приставку "город"
    mutate(city_name=stri_replace_first_regex(.$location_src, pattern="город\\s+", replacement="")) %>%
    select(city_name, lat, lon, api, formatted_address)

  df2
}

tic()
geo_df <- c(geoGoogle, geoYandex, geoOSM) %>%
  purrr::map_df(processGeoEncoding, cities_df$city_name)
toc()

write_csv(geo_df, "cities_geodata.csv")

# df <- geo_df %>% reshape2::recast(location_src ~ api + variable)

stop()

select(geo_df, -formatted_address)

df1 <- geo_df %>% 
  group_by(location_src) %>% 
  summarize(sd_lat=sd(lat), sd_lon=sd(lon)) %>% 
  ungroup() %>% 
  arrange(location_src)

df2 <- geo_df %>% 
  arrange(location_src, api)

l2 <- cities_df$city_name %>% purrr::map_df(safely(geoGoogle))

object_size(c(geoGoogle, geoYandex, geoOSM))

cities_df$city_name %>% processGeoEncoding("geoOSM")

# purr::map(c(geoGoogle, geoYandex, geoOSM)) cities_df$city_name %>%


m <- addr %>% 
  stri_replace_all_regex(pattern=c("(\\s+)", ","), replacement=c("+", ""), vectorize_all=FALSE)
# browser()

oldgeoYandex <- function(location, IsAddressFilter=TRUE){
  flog.info(paste0("==> function geoYandex, location = ", location))
  
  stopifnot(is.character(location))
  loc <- location
  if (IsAddressFilter == T) {
    IsAddress <- FALSE
    if (grepl(pattern = "\\bокр[уг]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bобл[асть]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bobl\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bg\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bг[ород]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bкр[ай]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bрес[публика]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bул[ица]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bul\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bкор[пус]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bкрп\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bд[ом]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bдер[евня]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bпр[роезд]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bпер[еулок]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bпр[оспект]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bpr\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bр-н\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bрайон\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bс[ело]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bп[оселок]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bмкр\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bш[оссе]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bбул[ьвар]*\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\bб-р\\b", x = loc))
      IsAddress <- TRUE
    if (grepl(pattern = "\\b\\d{1,4}\\b", x = loc))
      IsAddress <- TRUE
    if (IsAddress == FALSE) {
      return (
        tibble(
          request = loc,
          AdminAreaName = "IsAddress==F",
          LocalityName = "IsAddress==F",
          precision = "IsAddress==F",
          text = "IsAddress==F",
          name = "IsAddress==F",
          pos = NA,
          lon = NA,
          lat = NA
        )
      )
      # break
    }
  }
  
  # browser()
  location <- gsub(",", "", location)
  location <- gsub(" ", "+", location)
  url_string <- paste("http://geocode-maps.yandex.ru/1.x/?geocode=",
                      location,
                      sep = "")
  url_string <- URLencode(url_string)
  xmlText <- paste(readLines(url_string), "\n", collapse = "")
  data <- xmlParse(xmlText, asText = TRUE)
  xml_data <- xmlToList(data)
  AdminAreaName <-
    xml_data$GeoObjectCollection$featureMember$GeoObject$metaDataProperty$GeocoderMetaData$AddressDetails$Country$AdministrativeArea$AdministrativeAreaName
  LocalityName <-
    xml_data$GeoObjectCollection$featureMember$GeoObject$metaDataProperty$GeocoderMetaData$AddressDetails$Country$AdministrativeArea$SubAdministrativeArea$Locality$LocalityName
  precision <-
    xml_data$GeoObjectCollection$featureMember$GeoObject$metaDataProperty$GeocoderMetaData$precision
  text <-
    xml_data$GeoObjectCollection$featureMember$GeoObject$metaDataProperty$GeocoderMetaData$text
  name <- xml_data$GeoObjectCollection$featureMember$GeoObject$name
  pos <- xml_data$GeoObjectCollection$featureMember$GeoObject$Point$pos
  lon <-
    as.numeric(gsub(
      pattern = "(.+)\\s+(.+)",
      replacement = "\\1",
      x = pos
    ))
  lat <-
    as.numeric(gsub(
      pattern = "(.+)\\s+(.+)",
      replacement = "\\2",
      x = pos
    ))
  return (
    tibble(
      request = loc,
      AdminAreaName = AdminAreaName,
      LocalityName = LocalityName,
      precision = precision,
      text = text,
      name = name,
      pos = pos,
      lon = lon,
      lat = lat
    )
  )
}

stop()
#### Попытка распаралелить ####
# подготовка к параллельному запуску
gc()
nworkers <- detectCores() - 1
registerDoParallel(nworkers)
getDoParWorkers()

# регистрируем отдельный логгер на исполнителя
# http://stackoverflow.com/questions/38828344/how-to-log-when-using-foreach-print-or-futile-logger
loginit <- function(logfile) flog.appender(appender.file(logfile))
foreach(input=rep(common_log_name, nworkers),
        .packages='futile.logger') %do% loginit(input)


mtrace(geoYandex)
z <- Adr
system.time(
  l <- foreach(
    x = z,
    .combine = list,
    .multicombine = TRUE,
    .packages = c("rjson", "XML", "tibble")
  ) %do% {
    res <- geoYandex(x, IsAddressFilter = T)
    flog.info(paste0("address = ", x, " result = ", capture.output(str(res))))
    res
  })
mtrace.off()
# освобождаем параллель
registerDoSEQ() # http://stackoverflow.com/questions/25097729/un-register-a-doparallel-cluster


# список тестовых адресов ----------------
addr <-
  c(
    "брянская область, г. дятьково, ул. ленина, д. 166",
    "брянская область, г. дятьково, ул. ленина, д. 166",
    "г.дятьково, ул.ленина, д.166",
    "г.дятьково, ул.ленина, д.166, брянская обл.",
    "г.дятьково, ул.ленина, д.166 2",
    "г. дятьково,ул. ленина, д.166",
    "г.дятьково, ул.ленина, д.166",
    "г.дятьково, ул.ленина, д.166, брянская обл.",
    "г.дятьково, ул.ленина, д.166 2",
    "г.лобня, ул.горького, д.104",
    "г.котельники, мкр н силикат, д.29",
    "московская область, люберецкий район, г. котельники, мкр. силикат, дом 29",
    "московская область, люберецкий район, г. котельники, мкр. силикат, дом 29",
    "мо, г.лобня, ул.горького, д.104",
    "московская область, люберецкий район, поселок котельники, мкр н силикат, д.29",
    "г.лобня, ул.горького, д.104",
    "г.лобня, ул.горького, д.104",
    "г.лобня, ул.горького, д.104",
    "г.котельники, мкр н силикат, д.29",
    "г.москва, ул.судостроительная, д.1",
    "г. москва, судостроительная улица, д. 1",
    "г. москва, ул. судостроительная, д. 1",
    "г.москва, ул. судостроительная, д.1",
    "г.москва, ул.судостроительная, д.1",
    "г. москва, ул. судостроительная, д. 1",
    "г.москва, ул.судостроительная, д.1",
    "г. москва, судостроительная улица, д. 1",
    "г.москва, ул.судостроительная, д.1",
    "мо, дмитровский район, д. подосинки",
    "мо, дмитровский район, д. подосинки",
    "п.подосинки, г.п.дмитров",
    "п.подосинки, г.п.дмитров",
    "дмитровский р н, п.подосинки",
    "дмитровский р н, д.подосинки, г.п.дмитров",
    "дмитровский р н, п.подосинки",
    "московская область, дмитровский р н, п.подосинки",
    "подольский р н, с.вороново, магазин"
  )

