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


df0 <- readRDS("./Shiny_DPI_reports/edr_http.rds")

# facet графики для top 10 up/down ---------------------------------
t <- 9161234567
sprintf("(%s) %s-%s-%s", unlist(list(1, 2, 3, 4)))
st <- as.character(t)
m <- st %>% {sprintf("(%s) %s-%s-%s", stri_sub(., 1, 3), stri_sub(., 4, 5), stri_sub(., 6, 7), stri_sub(., 8, 9))}

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

hgroup.enum <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  
  tick_time <- date
  if (time.bin < 1 & !(time.bin %in% c(0.25, 0.5))) time.bin = 1
  n <- floor((hour(tick_time)*60 + minute(tick_time))/ (time.bin * 60))
  floor_date(tick_time, unit="day") + minutes(n * time.bin *60)
}


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
  