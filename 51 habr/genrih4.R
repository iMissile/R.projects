rm(list=ls()) # очистим все переменные
library(tibble)
library(rjson)
library(XML)
library(foreach)
library(debug)
library(doParallel)
library(futile.logger)

common_log_name <- "trace.log"

flog.appender(appender.file(common_log_name))
flog.threshold(TRACE)
flog.info("============= Classification started ===============")



Cluster_Id <- c("50494", "50494", "50494", "50494", "50494", "50494", "50494", "50494", "50494", "50863", "50863", 
"50863", "50863", "50863", "50863", "50863", "50863", "50863", "50863", "51073", "51073", "51073", 
"51073", "51073", "51073", "51073", "51073", "51073", "51300", "51300", "51300", "51300", "51300",
"51300", "51300", "51300", "51473")
Adr <-
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
#### Геокодер########
# количестве запросов в сутки - до 25 000




geoYandex <- function(location, IsAddressFilter = TRUE){
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

### Простой тест ####
# system.time(l <- lapply(Adr, geoYandex, IsAddressFilter = T))

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
