library(tidyverse)
library(forcats)
library(magrittr)
library(stringi)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(hrbrthemes)
library(lubridate)
library(profvis)
library(RcppRoll)
library(digest)
library(xts)
library(padr)
library(zoo)
library(forecast)
library(fuzzyjoin)

source("./Shiny_DPI_reports/funcs.R")

df0 <- readRDS("./Shiny_DPI_reports/edr_http_small.rds")

if(FALSE){
# facet графики для top 10 up/down ---------------------------------
t <- 9161234567
# sprintf("(%s) %s-%s-%s", list(1, 2, 3, 4)) # не работает, пишет `too few arguments`
st <- as.character(t)
m <- st %>% {sprintf("(%s) %s-%s-%s", stri_sub(., 1, 3), stri_sub(., 4, 6), stri_sub(., 7, 8), stri_sub(., 9, 10))}

df1 <- df0 %>%
  select(timestamp=end_timestamp, downlink_bytes, uplink_bytes, site, msisdn) %>%
  gather(downlink_bytes, uplink_bytes, key="direction", value="bytes") %>%
  sample_frac(0.2) %>%
  filter(bytes>0.1)


group_df <- df0 %>%
  mutate(msisdn=as.factor(msisdn)) %>%
  group_by(msisdn) %>%
  summarise(user_recs=n(), 
            uplink_Kb=round(sum(uplink_bytes)/1024, 1), 
            downlink_Kb=round(sum(downlink_bytes)/1024, 1)) %>%
  arrange(desc(user_recs)) %>%
  mutate(msisdn_chr=as.character(msisdn) %>% 
           {sprintf("(%s) %s-%s-%s", stri_sub(., 1, 3), 
                    stri_sub(., 4, 5), stri_sub(., 6, 7), stri_sub(., 8, 9))})
         

plot_df <- group_df %>%
  gather(downlink_Kb, uplink_Kb, key="direction", value="volume") %>%
  group_by(direction) %>%
  top_n(10, volume) # объем считается в килобайтах
  #mutate(downlink_Mb=downlink_Kb/1024) %>%
  #arrange(desc(downlink_Kb))

gp <- ggplot(plot_df, aes(fct_reorder(msisdn, volume), volume)) + 
  geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
  facet_grid(.~direction, scales="free") +
  theme_ipsum_rc(base_size=16, axis_title_size=14) +
  xlab("'Как бы' MSISN") +
  ylab("Суммарный Downlink, Mb") +
  coord_flip()

gp

# выборка top 10 для площадок, up/down ---------------------------------

regions <- c("Владивосток", "Новосибирск", "Екатеринбург", "Н.Новгород", "Краснодар", "Москва", "Санкт-Петербург")
subregions <- c("Краснодар", "Москва")

df2 <- df0 %>%
  select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, msisdn) %>%
  gather(up, down, key="direction", value="bytes") %>%
  group_by(site, direction, msisdn) %>%
  summarise(user_recs=n(), bytes=sum(bytes)) %>%
  top_n(10, bytes) %>%
  ungroup() # %>% spread(direction, bytes)

# spread дает на 4 меньше, поскольку в нескольких местах один и тот же msisdn присутствует и в up и в down
# df2 %>% filter(!is.na(downlink_bytes) & !is.na(uplink_bytes))


# рисуем time-series трафик ---------------------------------

# hgroup.enum <- function(date, time.bin = 4){
#   # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
#   # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
#   # отсчет измерений идет с 0:00
#   # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
#   # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
#   
#   tick_time <- date
#   if (time.bin < 1 & !(time.bin %in% c(0.25, 0.5))) time.bin = 1
#   n <- floor((hour(tick_time)*60 + minute(tick_time))/ (time.bin * 60))
#   floor_date(tick_time, unit="day") + minutes(n * time.bin *60)
# }

# ломаный сэмпл
# https://edwinth.github.io/blog/padr-examples/
plot_df <- df0 %>%
  mutate(msisdn=sample(100:199, nrow(.), replace=TRUE)) %>%
  select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, msisdn)

saveRDS(plot_df, "padr_sample.rds")
write_csv(plot_df, "padr_sample.csv")

plot_df <- readRDS("padr_sample.rds")
tmp_df <- plot_df %>%
  thicken("day", col="time") # don't work


# https://edwinth.github.io/blog/padr-examples/
plot_df <- df0 %>%
  filter(site=="Москва") %>%
  select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, msisdn) %>%
  mutate(timegroup=hgroup.enum(timestamp, time.bin=24)) %>%
  # thicken("day", col="time") %>% # не работает
  group_by(site, timegroup) %>%
  summarize(up=sum(up), down=sum(down)) %>%
  ungroup() %>%
  gather(up, down, key="direction", value="volume") %>%
  mutate(volume=volume/1024/1024) %>% #пересчитали в Мб
  group_by(site, direction) %>%
  mutate(volume_meanr = RcppRoll::roll_meanr(x=volume, n=7, fill=NA)) %>%
  ungroup()
  # filter(volume>1)

windowsFonts(robotoC="Roboto Condensed")

gp <- ggplot(plot_df, aes(timegroup, volume)) + 
  # http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
  facet_wrap(~site, nrow=2) +
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  scale_color_brewer(palette="Set1",
                     name="Трафик",
                     breaks=c("up", "down"),
                     labels=c("Uplink", "Downlink")
                     ) +
  # geom_line(aes(colour=direction), alpha=0.4, lwd=1) +
  geom_line(aes(y=volume_meanr, colour=direction), alpha=0.6, lwd=1) +
  geom_point(aes(colour=direction), alpha=0.6, shape=1, size=3) +
  scale_y_continuous(trans='log10') +
  #scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
  #              labels=trans_format("log10", math_format(10^.x))) +
  #annotation_logticks() +
  # theme_ipsum_rc(base_size=16, axis_title_size=14) +
  theme_ipsum_rc(base_family="robotoC", base_size=16, axis_title_size=14) +
  xlab("Дата, время") +
  ylab("Суммарный объем данных, Mb")

gp

# расчет среднего по всему объему для группы с разбросом по группам ------------------------------------
t <- traffic_df()
m <- t %>% group_by(timegroup) %>% nest()

m5 <- t %>% group_by(timegroup) %>%
  mutate(meanr=mean(volume_meanr)) %>%
  ungroup() %>%
  arrange(timegroup)

m2 <- m %>%
  mutate(global_meanr=purrr::map(data, ~ mean(.x$volume_meanr)))
# пока не получается потом развернуть
m3 <- m2 %>% ungroup() %>% unnest(data)

# категоризируем http_hosts ------------------------------------
repl_df <- tribble(
  ~pattern, ~category,
  "instagramm\\.com", "Instagramm",
  "vk\\.com", "ВКонтакте",
  "xxx", "XXX",
  "facebook.com", "Facebook",
  "twitter.com", "Twitter",
  "windowsupdate\\.com", "Microsoft" 
)

t <- df0 %>% group_by(http_host) %>%
  summarise(n=n())

t2 <- regex_left_join(t, repl_df, by=c(http_host="pattern"))

}
# -----------------------------------------------------------------
# суточное прогнозирование средствами пакета forecast --------------
# повторяем по мотивам \iwork.HG\R.projects\02 predictive_PM_demo\predict_interface_load_06(tadv_test)

df1 <- df0 %>%
  filter(site=="Москва") %>%
  select(timestamp=end_timestamp, down=downlink_bytes, up=uplink_bytes, site, msisdn) %>%
  mutate(timegroup=hgroup.enum(timestamp, time.bin=24)) %>%
  # thicken("day", col="time") %>% # не работает
  group_by(site, timegroup) %>%
  summarize(up=sum(up), down=sum(down)) %>%
  ungroup()

f_depth <- 7 # глубина прогноза


# 1. есть нюансы с отсутствующими датами. Для получения правильного прогноза обязательно необходимо вставить 
# все даты с заданной периодичностью и заполнить значения NA

df2 <- df1 %>% mutate(value=up) %>% # прогнозирование делаем для value
  # pad(interval="day", start_val=min(df1$timegroup)-days(7))
  pad(interval="day") %>% # добили отсутствующие даты
  head(-1) # убрали частичный недоагрегат по последней дате


# скрываем эксперименты, пишем тут начистую

# строим формальный сенсор без привязки к дате, с указанием недельной периодичности
# почему берем 7 сказано в "Forecasting with daily data" http://robjhyndman.com/hyndsight/dailydata/
# frequency	-- the number of observations per unit of time.
sensor <- ts(df2$value, frequency=7)

#  ARIMA = Auto Regressive Integrated Moving Average
# увы, ARIMA работает только для суточных измерений с глубиной не более 350 измерений
# ограничение в 350: http://robjhyndman.com/hyndsight/longseasonality/
# ==============================================
fit <- auto.arima(sensor) #http://stackoverflow.com/questions/14331314/time-series-prediction-of-daily-data-of-a-month-using-arima
# fit <- tbats(sensor)
fcast <- forecast(fit, h=f_depth) # h - Number of periods for forecasting

# теперь ручками пересоберем данные в удобном для визуализации виде
# fcast$x -- исходные данные, fcast$mean -- прогнозные

tseq <- seq(max(df2$timegroup) + days(1), along.with=fcast$mean, by = "1 day")

df3 <-
  tibble(
    timegroup=tseq,
    value=fcast$mean,
    # 80%
    low1=fcast$lower[, 1], 
    upp1=fcast$upper[, 1],
    # 95%
    low2=fcast$lower[, 2], 
    upp2=fcast$upper[, 2]
  )

cpal <- brewer.pal(7, "Blues")
cpal <- brewer.pal(7, "Oranges")

gp <- ggplot(df3, aes(timegroup, value)) +
  #geom_ribbon(aes(ymin=low2, ymax=upp2), fill="yellow") +
  #geom_ribbon(aes(ymin=low1, ymax=upp1), fill="orange") +
  # рисуем исходные данные
  geom_line(data=df2, aes(timegroup, value)) +
  geom_ribbon(aes(ymin=low2, ymax=upp2), fill=cpal[1]) +
  geom_ribbon(aes(ymin=low1, ymax=upp1), fill=cpal[2]) +
  theme_bw() +
  geom_line() +
  # geom_line(data=df[!is.na(df$forecast), ], aes(time, forecast), color="blue", size=1.5, na.rm=TRUE) +
  # http://docs.ggplot2.org/current/scale_continuous.html
  #scale_x_continuous("") +
  #scale_y_continuous(limits=c(0, 120)) +
  # scale_x_date(breaks=date_breaks('days'), labels = date_format("%d %b %Y")) +
  # ОБЯЗАТЕЛЬНО library(scales)
  # http://docs.ggplot2.org/current/scale_date.html
  scale_x_datetime(labels=date_format("%d %b %Y"), breaks=date_breaks("day")) +
  # ylim(0, NA) +
  # scale_y_continuous(trans='log10', limits = c(0, NA)) +
  scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
                labels=trans_format("log10", math_format(10^.x)),
                limits=c(1, 10^6)) +
  xlab("Дата") +
  ylab("Up volume") +
  # scale_shape(solid = FALSE) +
  # http://blog.rstudio.org/2012/09/07/ggplot2-0-9-2/  
  # opts(title=paste("Forecasts from ", fcast$method))
  labs(title=paste("Forecasts from ", fcast$method))

gp

if(FALSE){
start_date <- min(df2$timegroup) # дата последующей привязки
  
# tdf<- tibble(timegroup=seq(min(df1$timegroup), max(df1$timegroup) + days(f_depth), by="1 day"))

# разные способы объединения
# merge(df2, tdf, by="timegroup", all.x=TRUE, all.y=TRUE)
# res <- full_join(df2, tdf, by="timegroup")


# строим формальный сенсор без привязки к дате, с указанием недельной периодичности
# почему берем 7 сказано в "Forecasting with daily data" http://robjhyndman.com/hyndsight/dailydata/
# frequency	-- the number of observations per unit of time.
sensor <- ts(df2$value, frequency=7)
#sensor <- head(sensor, -1) # убрали частичный недоагрегат по последней дате

#  ARIMA = Auto Regressive Integrated Moving Average
# увы, ARIMA работает только для суточных измерений с глубиной не более 350 измерений
# ограничение в 350: http://robjhyndman.com/hyndsight/longseasonality/
# ==============================================
fit <- auto.arima(sensor) #http://stackoverflow.com/questions/14331314/time-series-prediction-of-daily-data-of-a-month-using-arima
fit
# fit <- tbats(sensor)
fcast <- forecast(fit, h=f_depth) # h - Number of periods for forecasting

# Directly plot your forecast without your axes
autoplot(fcast) +
geom_forecast(h=f_depth, level=c(50,80,95)) + theme_bw()

autoplot(fcast)

#ggplot(data=df1, aes(x=timegroup, y=up)) +

# теперь ручками пересоберем данные в удобном для визуализации виде
# fcast$x -- исходные данные, fcast$mean -- прогнозные

fcast$timegroup <- seq(max(df2$timegroup) + days(1), along.with=fcast$mean, by = "1 day")

lenx <- length(fcast$x)
lenmn <- length(fcast$mean)

df3 <-
  tibble(
    timegroup = seq(start_date, length.out = lenx + lenmn, by = "1 day"),
    value = c(fcast$x, fcast$mean),
    # type=c(rep("real", lenx), rep("fcast", lenmn)),
    forecast = c(rep(NA, lenx), fcast$mean),
    low1 = c(rep(NA, lenx), fcast$lower[, 1]),
    upp1 = c(rep(NA, lenx), fcast$upper[, 1]),
    low2 = c(rep(NA, lenx), fcast$lower[, 2]),
    upp2 = c(rep(NA, lenx), fcast$upper[, 2])
  )

cpal <- brewer.pal(7, "Blues")
cpal <- brewer.pal(7, "Oranges")



gp <- ggplot(df3, aes(timegroup, value)) +
  #geom_ribbon(aes(ymin=low2, ymax=upp2), fill="yellow") +
  #geom_ribbon(aes(ymin=low1, ymax=upp1), fill="orange") +
  geom_ribbon(aes(ymin=low2, ymax=upp2), fill=cpal[1]) +
  geom_ribbon(aes(ymin=low1, ymax=upp1), fill=cpal[2]) +
  theme_bw() +
  geom_line() +
  # geom_line(data=df[!is.na(df$forecast), ], aes(time, forecast), color="blue", size=1.5, na.rm=TRUE) +
  # http://docs.ggplot2.org/current/scale_continuous.html
  #scale_x_continuous("") +
  #scale_y_continuous(limits=c(0, 120)) +
  # scale_x_date(breaks=date_breaks('days'), labels = date_format("%d %b %Y")) +
  # ОБЯЗАТЕЛЬНО library(scales)
  # http://docs.ggplot2.org/current/scale_date.html
  scale_x_datetime(labels=date_format("%d %b %Y"), breaks=date_breaks("week")) +
  ylim(0, NA) +
  xlab("Дата") +
  ylab("Up volume") +
  # scale_shape(solid = FALSE) +
  # http://blog.rstudio.org/2012/09/07/ggplot2-0-9-2/  
  # opts(title=paste("Forecasts from ", fcast$method))
  labs(title=paste("Forecasts from ", fcast$method))

gp
}
  