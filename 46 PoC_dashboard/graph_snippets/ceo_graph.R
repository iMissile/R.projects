rm(list=ls()) # очистим все переменные

library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
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

getwd()
source("common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

# main ================================================================

df <- get_weather_df(back_days = 7, forward_days = 3)

# сделаем выгрузку в json --------------------------------------------------------

# df2 <- data.frame(timestamp = round(as.numeric(outw.df$timegroup), 0), 
#                   air_temp_past = ifelse(outw.df$time.pos == "PAST", round(outw.df$temp, 1), NA),
#                   air_temp_future = ifelse(outw.df$time.pos == "FUTURE", round(outw.df$temp, 1), NA),
#                   air_humidity_past = ifelse(outw.df$time.pos == "PAST", round(outw.df$humidity, 1), NA),
#                   air_humidity_future = ifelse(outw.df$time.pos == "FUTURE", round(outw.df$humidity, 1), NA)) %>%
#   arrange(timestamp)

df3 <- with(df, {
  data.frame(timestamp = round(as.numeric(timegroup), 0), 
                  air_temp_past = ifelse(time.pos == "PAST", round(temp, 1), NA),
                  air_temp_future = ifelse(time.pos == "FUTURE", round(temp, 1), NA),
                  air_humidity_past = ifelse(time.pos == "PAST", round(humidity, 1), NA),
                  air_humidity_future = ifelse(time.pos == "FUTURE", round(humidity, 1), NA)) %>%
  arrange(timestamp)
})

x <- jsonlite::toJSON(list(results = df3), pretty = TRUE)
write(x, file="./export/real_weather.json")

# отобразим для себя --------------------------------------------------------

# https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
p1 <- ggplot(df, aes(timegroup, temp, colour = time.pos)) +
  # ggtitle("График температуры") +
  # scale_fill_brewer(palette="Set1") +
  # scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  # geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
  # geom_point(shape = 1, size = 3) +
  # geom_line(lwd = 1, linetype = 'dashed', color = "red") +
  scale_x_datetime(labels = date_format("%d.%m"), breaks = date_breaks("1 days"), minor_breaks = date_breaks("6 hours")) +
  geom_line(lwd = 1.2) +
  theme_igray() +
  theme(legend.position="none") +
  xlab("Дата") +
  ylab("Температура, град. C")

p1
