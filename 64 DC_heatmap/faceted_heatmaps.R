library(tidyverse)
library(lubridate)   # date manipulation
library(magrittr)
library(countrycode) # turn country codes into pretty names
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
# library(viridis)     # best. color. palette. evar.

attacks <- read_csv("./data/eventlog.csv", col_types="ccc", progress=interactive()) %>%
  slice(1:20)
  #sample_n(20, replace=FALSE)


if(FALSE){
# рабочее решение в старой парадигме отсюда: https://rud.is/projects/facetedheatmaps.html
parseDayParts <- function(cc, ts, tz) {
  real_times <- ymd_hms(ts, tz=tz[1], quiet=TRUE)
  data_frame(timestamp=ts,
             source_country=cc, 
             wkday=weekdays(as.Date(real_times, tz=tz[1])),
             hour=format(real_times, "%H", tz=tz[1]))
}

attacks2 <- attacks %>%
group_by(tz) %>%
  do(parseDayParts(.$source_country, .$timestamp, .$tz)) %>% 
  ungroup()
}

# рабочее решение отсюда: https://github.com/rstudio/rstudio-conf/tree/master/2017/List_Columns-Jenny_Bryan
attacks3 <- attacks %>% 
  mutate(rt=map(.$timestamp, ~ ymd_hms(.x, quiet=FALSE))) %>%
  mutate(hour=map2(.$rt, .$tz, ~ format(.x, "%H", tz=.y))) %>%
  mutate(wkday=map2(.$rt, .$tz, ~ weekdays(as.Date(.x, tz=.y)))) %>%
  unnest(rt, hour, wkday)




stop()

# ================ ничего так и не работает нормально с датами....

# проверим работу lubridate
# ymd_hms(attacks$timestamp[1], tz=attacks$tz[1])

# нам надо все привести к локальному времени, чтобы отмапировать на рабочие часы
# 2015-03-12T16:17:42.660169Z, CN, Asia/Shanghai = 2015-03-12T16:17:42.000+08:00
# as.POSIXct(as.numeric(as.POSIXct(1426175957, tz="Asia/Shanghai", origin="1970-01-01")), origin="1970-01-01")

# векторизируем с помощью враппера, чтобы применить внутри mutate
parseDate <- Vectorize(function(x, tz){
  # нам надо все привести к локальному времени, чтобы отмапировать на рабочие часы, 
  # поэтому принудительно используем TZ
  ymd_hms(x) #, tz=tz)
})

attacks2 <- attacks %>% 
  mutate(rt=parseDate(timestamp, tz))%>%
  mutate(local_time=as.POSIXct(rt, origin="1970-01-01", tz=tz)) %>%
  mutate(hour=lubridate::hour(local_time)) %>%
  mutate(wkday=lubridate::wday(local_time))
  

#map2_dbl -- возвращает вектор, а не список
attacks3 <- attacks %>% 
  mutate(rt=map2_dbl(.$timestamp, .$tz, ~ ymd_hms(.x, tz=.y, quiet=FALSE))) %>%
  mutate(local_time=map2(.$rt, .$tz, ~ as.POSIXct(.x, tz=.y, origin="1970-01-01"))) %>%   # as.POSIXct(rt, origin="1970-01-01", tz=tz)) %>%
  mutate(hour=lubridate::hour(local_time)) %>%
  # mutate(wkday=lubridate::wday(local_time)) %>%
  #mutate(rt=map_dbl(.$timestamp, ~ ymd_hms(.x, quiet=FALSE))) %>%
  #mutate(real_times=as.POSIXct(rt, tz=.$tz, origin="1970-01-01"))
  mutate(text=unlist(map2(.$timestamp, .$tz, ~ paste0(.x, ' - ', .y))))

