rm(list=ls()) # ������� ��� ����������
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

# ��������� ������� �������
cities_df <- read_excel("./data/������.xlsx") %>%
  select(city_name=`����� (���)`) %>%
  # head(5) %>%
  # add_row(city_name="�������", .before=1) %>%
  # add_row(city_name="�������� ���", .before=1) %>%
  distinct()
  
# ��������� -------
geoOSM <- function(location){
  #' ���������� �������� � ����� - �� 25 000 
  # ���������� location ������ � ������������� � ����� ������, ����������� `+`
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
  #' ���������� �������� � ����� - �� 25 000 
  # ���������� location ������ � ������������� � ����� ������, ����������� `+`
  loc <- location %>% 
    stri_replace_all_regex(pattern=c("(\\s+)", ","),
                           replacement=c("+", ""), 
                           vectorize_all=FALSE)
  ## ����� �� ������������ crul, ��� 11
  # query params are URL encoded for you, so DO NOT do it yourself
  ## if you url encode yourself, it gets double encoded, and that's bad
  base_url <- "http://geocode-maps.yandex.ru/1.x/?geocode="
  geo_url <- paste0(base_url, loc, "&format=json")
  # r <- fromJSON(content(httr::GET(geo_url, verbose()), as='text'))
  r <- fromJSON(content(httr::GET(geo_url), as='text'))
  # � ������ ������ ������ ����� ��������� ������� � �������. 
  # ��������, �� ������ �� ���� � ����� � �������� � ����

  # �������������� ����������, ����� ������, ��������� ������ ������
  # https://tech.yandex.ru/maps/doc/geocoder/desc/reference/GeocoderResponseMetaData-docpage/
  meta <- r$response$GeoObjectCollection$metaDataProperty$GeocoderResponseMetaData
  flog.info(paste0("Yandex Geo_API response: request=", meta$request, ", found=", meta$found))
  
  # ������� ������ ������� �� ���������, ������ ����������� �����
  # �������� �� ������ "����� �������" � ����� �������� � "�����"
  obj <- r$response$GeoObjectCollection$featureMember %>% head(1)
  # ������� (lon, lat)
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
  # ���������� location ������ � ������������� � ����� ������, ����������� `+`
  loc <- location %>% 
    stri_replace_all_regex(pattern=c("(\\s+)", ","), 
                           replacement=c("+", ""), 
                           vectorize_all=FALSE)

  base_url <- "http://maps.googleapis.com/maps/api/geocode/json?address="
  geo_url <- paste0(base_url, loc, "&language=ru")
  r <- fromJSON(content(httr::GET(geo_url), as='text'))
  flog.info(paste0("Google Geo_API response: request=", location, ", status=", r$status))

  # ���� ���������, ��� status="OK". ���� ����� ������, �� status="ZERO_RESULTS"
  # browser()
  res <- tibble(location_src=location,
                api="geoGoogle",
                formatted_address=r$results$formatted_address,
                lat=r$results$geometry$location$lat,
                lon=r$results$geometry$location$lng)
}

# �������� ----------------
# l <- addr %>% purrr::map(geoYandex, IsAddressFilter=T)

processGeoEncoding <- function(fun, city_name) {
  # browser()
  res <- city_name %>%
    {stri_join("����� ", .)} %>% 
    # purrr::map(safely(geoGoogle)) %>%
    # purrr::map(safely(geoYandex)) %>%
    # purrr::map(safely(geoOSM)) %>%
    purrr::map(safely(fun)) %>% 
    transpose()
  # �������� ������ �� NULL ���������� � �������� �� 
  ok <- res$error %>% map_lgl(is_null)
  df2 <- res$result %>% 
    keep(ok) %>% 
    {tibble(val=.)} %>% 
    unnest() %>%
    # ������� ����� ��������� "�����"
    mutate(city_name=stri_replace_first_regex(.$location_src, pattern="�����\\s+", replacement="")) %>%
    select(city_name, lat, lon, api, formatted_address)

  df2
}

tic()
geo_df <- c(geoGoogle, geoYandex, geoOSM) %>%
  purrr::map_df(processGeoEncoding, cities_df$city_name)
toc()

write_csv(geo_df, "cities_geodata.csv")
saveRDS(geo_df, "cities_geodata.rds")

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
