library(tidyverse)
library(lubridate)   # date manipulation
library(magrittr)
library(countrycode) # turn country codes into pretty names

attacks_raw <- read_csv("./data/eventlog.csv", col_types="ccc", progress=interactive()) %>%
  slice(1:100000)

# POSIXct -- вектор с атрибутом таймзоны. 
# Поэтому сложно векторизовать для разных tz

# так считает 60 секунд ======================
# рабочее решение отсюда: https://github.com/rstudio/rstudio-conf/tree/master/2017/List_Columns-Jenny_Bryan
attacks <- attacks_raw %>% 
  # mutate(rt=map(.$timestamp, ~ ymd_hms(.x, quiet=FALSE))) %>% # с этим подходом ~ в 8 раз медленнее
  mutate(rt=ymd_hms(timestamp, quiet=FALSE)) %>%
  mutate(hour=as.numeric(map2(.$rt, .$tz, ~ format(.x, "%H", tz=.y)))) %>%
  # mutate(hour2=lubridate::hour(rt)) %>%
  mutate(wkday_text=map2(.$rt, .$tz, ~ weekdays(as.Date(.x, tz=.y)))) %>%
  unnest(rt, hour, wkday_text) %>%
  # превратим wkday в фактор принудительно с понедельника
  mutate(wkday=factor(wkday_text, levels=weekdays(dmy("13.02.2017")+0:6)))

# так считает 1.5 секунды ===================
parseDFDayParts <- function(df, tz) {
  real_times <- ymd_hms(df$timestamp, tz=tz, quiet=TRUE)
  tibble(wkday=weekdays(as.Date(real_times, tz=tz)),
         hour=as.numeric(format(real_times, "%H", tz=tz)))
}

attacks2 <- attacks_raw %>%
  group_by(tz) %>%
  nest()

attacks2 <- attacks2 %>%
  mutate(res=map2(data, tz, parseDFDayParts)) %>%
  unnest() %>%
  # превратим wkday в фактор принудительно с понедельника
  mutate(wkday=factor(wkday, levels=weekdays(dmy("13.02.2017")+0:6)))

# ======================================