# генерируем тестовые файлы для отладки интерфейса
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(readr)
library(jsonlite)
library(magrittr)
library(httr)
library(ggthemes)
library(ggmap)
library(RColorBrewer) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
# library(KernSmooth)
library(akima)
library(rdrop2)
# library(rgl)

min_lim <- ceiling_date(now() - days(3), unit = "day")
max_lim <- floor_date(now() + days(2), unit = "day")
lims <- c(min_lim, max_lim)

# Setting limits with scale_x_datetime and time data
# http://stackoverflow.com/questions/30607514/setting-limits-with-scale-x-datetime-and-time-data

hgroup.enum <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения. 
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  tick_time <- date
  n <- floor(hour(tick_time) / time.bin)
  floor_date(tick_time, unit = "day") + hours(n * time.bin)
}

generate_raw_data <- function() {
  # генерируем погодные даты
  
  tick.seq <- seq(min_lim, max_lim, by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  
  # для подготовки данных 
  # собираем сразу data.frame: время, #сенсора, показание
  n <- length(tick.seq)
  mydata <- data.frame(timestamp = tick.seq,
                       temp.min = rnorm(n, 14, 3), # используем методику дополнения
                       pressure = rnorm(n, 750, 30),
                       humidity = runif(n, 10, 100),
                       rain = runif(n, 0, 20))
  
  # максимальная температура должна быть выше минимальной
  # средняя должна быть между максимальной и минимальной
  mydata %<>% mutate(temp.max = temp.min + runif(n, 5, 15)) %>%
    mutate(temp = temp.min + 0.7 * runif(n, 0, temp.max - temp.min)) %>%
    mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) %>%
    mutate(date = date(timestamp))
    
  
  mydata['time.pos'] <- ifelse(mydata$timestamp < now(), "PAST", "FUTURE")
  mydata
}

df <- generate_raw_data()


# https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
p1 <- ggplot(df, aes(timegroup, temp, colour = time.pos)) +
  # ggtitle("График температуры") +
  # scale_fill_brewer(palette="Set1") +
  # scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  # geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
  # geom_point(shape = 1, size = 3) +
  # geom_line(lwd = 1, linetype = 'dashed', color = "red") +
  scale_x_datetime(labels = date_format("%d.%m %H:%M", tz = "Europe/Moscow"), 
                   breaks = date_breaks("1 days"), 
                   #minor_breaks = date_breaks("6 hours"),
                   limits = lims
  ) +
  geom_line(lwd = 1.2) +
  #theme_igray() +
  theme(legend.position="none") +
  xlab("Дата") +
  ylab("Температура, град. C")


p2 <- ggplot(df, aes(timegroup, humidity, colour = time.pos)) +
  # ggtitle("График температуры") +
  # scale_fill_brewer(palette="Set1") +
  # scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Set2") +
  # geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
  # geom_point(shape = 1, size = 3) +
  # geom_line(lwd = 1, linetype = 'dashed', color = "red") +
  scale_x_datetime(labels = date_format("%d.%m %H:%M", tz = "Europe/Moscow"), 
                   breaks = date_breaks("1 days"), 
                   #minor_breaks = date_breaks("6 hours"),
                   limits = lims
  ) +
  geom_line(lwd = 1.2) +
  #theme_igray() +
  theme(legend.position="none") +
  ylim(0, 100) +
  xlab("Дата") +
  ylab("Влажность воздуха, %")


# запрос и формирование данных по осадкам (прошлое и прогноз) =====================================

plot_palette <- brewer.pal(n = 8, name = "Paired") 

df2 <- df %>%
  group_by(date) %>%
  summarise(rain = mean(rain))

p3 <- ggplot(df2, aes(date, rain)) +
  # ggtitle("График температуры") +
  # scale_fill_brewer(palette="Set1") +
  # scale_fill_brewer(palette = "Paired") +
  # scale_color_brewer(palette = "Set2") +
  geom_bar(fill = plot_palette[7], stat="identity") +
  # geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
  # geom_point(shape = 1, size = 3) +
  # geom_line(lwd = 1, linetype = 'dashed', color = "red") +
  scale_x_date(labels = date_format("%d.%m", tz = "Europe/Moscow"), 
               breaks = date_breaks("1 days"),
               limits = as.Date(lims, tz = "Europe/Moscow")
               ) +
  # geom_line(lwd = 1.2) +
  theme_igray() +
  theme(legend.position="none") +
  ylim(0, NA) +
  xlab("Дата") +
  ylab("Осадки (дождь), мм")

# grid.arrange(p1, p2, p3, ncol = 1) # возвращаем ggplot
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))
