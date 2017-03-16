library(tidyverse)
library(lubridate)   # date manipulation
library(magrittr)
library(countrycode) # turn country codes into pretty names
library(scales)      # pairs nicely with ggplot2 for plot label formatting
library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.
library(RColorBrewer)# best. color. palette. evar.
library(extrafont)   # http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
library(dvtiot)

# см. https://rpubs.com/omicsdata/faceted_heatmap

attacks_raw <- read_csv("./data/eventlog.csv", col_types="ccc", progress=interactive()) %>%
  slice(1:20000)
  #sample_n(20, replace=FALSE)

# ================== решение из статьи, переписанной мной на nest\map идеологию
parseDFDayParts <- function(df, tz) {
  real_times <- ymd_hms(df$timestamp, tz=tz, quiet=TRUE)
  tibble(wkday=weekdays(as.Date(real_times, tz=tz)),
         hour=as.numeric(format(real_times, "%H", tz=tz)))
}

attacks <- attacks_raw %>%
  group_by(tz) %>%
  nest()

attacks <- attacks %>%
  mutate(res=map2(data, tz, parseDFDayParts)) %>%
  unnest() %>%
  # превратим wkday в фактор принудительно с понедельника
  mutate(wkday=factor(wkday, levels=weekdays(dmy("13.02.2017")+0:6)))


wkdays <- count(attacks, wkday, hour)
wkdays

# разбираемс€ с кастомными шрифтами
# font_import(paths = NULL, recursive = TRUE, prompt = TRUE, pattern = NULL)
# In doing this your specifying explicitly the Windows Font mapping.
# windowsFonts(Times=windowsFont("TT Times New Roman"))
windowsFonts(Verdana="TT Verdana")


gg = ggplot(wkdays, aes(x=hour, y=wkday, fill=n))
gg = gg + geom_tile(color="white", size=0.1)
gg = gg + scale_fill_viridis(option="B", name="KPI", label=comma) # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
#gg = gg + scale_fill_distiller(palette="RdYlGn", name="# Events", label=comma) # http://docs.ggplot2.org/current/scale_brewer.html
#gg = gg + coord_equal()
gg = gg + coord_fixed(ratio = 1)
gg = gg + labs(x=NULL, y=NULL, title="KPI по часам и дн€м недели")
gg = gg + theme_tufte(base_family="Verdana")
gg = gg + theme(plot.title=element_text(hjust=0))

stop()

# ================== решение из статьи
if(TRUE){
  # рабочее решение в старой парадигме отсюда: https://rud.is/projects/facetedheatmaps.html
  # но это работает очень быстро, ~ 100 раз быстрее
  parseDayParts <- function(cc, ts, tz) {
    real_times <- ymd_hms(ts, tz=tz[1], quiet=TRUE)
    data_frame(timestamp=ts,
               source_country=cc, 
               wkday=weekdays(as.Date(real_times, tz=tz[1])),
               hour=format(real_times, "%H", tz=tz[1]))
  }
  
  attacks2 <- attacks_raw %>%
    group_by(tz) %>%
    do(parseDayParts(.$source_country, .$timestamp, .$tz)) %>% 
    ungroup()
}

# ==================неэффективное применение map!!!!¬се из-за tz и поштучной обработки
if(FALSE){
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
}



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

