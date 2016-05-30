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
library(viridis) # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
# library(KernSmooth)
library(akima)
library(rdrop2)
# library(rgl)

min_lim <- ceiling_date(now() - days(1), unit = "day")
max_lim <- floor_date(now() + days(1), unit = "day")
lims <- c(min_lim, max_lim)

# Setting limits with scale_x_datetime and time data
# http://stackoverflow.com/questions/30607514/setting-limits-with-scale-x-datetime-and-time-data
# Error: Invalid input: time_trans works with objects of class POSIXct only
# force_tz -- сюда же относится

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
                       rain = runif(n, 0, 5))
  
  m2 <- data.frame(timestamp = tick.seq,
                   rain2 = runif(n, 0, 5)) %>%
    arrange(desc(timestamp))
  
  browser()
  # а теперь объединим данные
  m3 <- left_join(mydata, m2, by = "timestamp")
  
  
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
df2 <- df %>%
  group_by(date) %>%
  summarise(rain = mean(rain)) %>%
  mutate(timestamp = force_tz(with_tz(as.POSIXct(date), tz = "GMT"), tz = "Europe/Moscow"))

# http://moderndata.plot.ly/create-colorful-graphs-in-r-with-rcolorbrewer-and-plotly/
plot_palette <- brewer.pal(n = 8, name = "Paired")


# схлопнем рисование графика
## brewer.pal.info
# https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
pp <- ggplot(df) +
  # ggtitle("График температуры") +
  # scale_fill_brewer(palette="Set1") +
  # scale_fill_brewer(palette = "Paired") +
  # geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
  # geom_point(shape = 1, size = 3) +
  # geom_line(lwd = 1, linetype = 'dashed', color = "red") +
  scale_x_datetime(labels = date_format("%d.%m %H:%M", tz = "Europe/Moscow"), 
                   breaks = date_breaks("1 days"), 
                   #minor_breaks = date_breaks("6 hours"),
                   limits = lims) +
  theme_igray() +
  theme(legend.position="none") +
  geom_vline(xintercept = as.numeric(now()), linetype = "dotted", color = "yellowgreen", lwd = 1.1) +
  xlab("Дата")
  
p1 <- pp +
  geom_line(aes(timegroup, temp, colour = time.pos), lwd = 1.2) +
  scale_color_manual(values = brewer.pal(n = 9, name = "Oranges")[c(3, 7)]) +
  ylab("Температура,\n град. C")
p2 <- pp +
  geom_line(aes(timegroup, humidity, colour = time.pos), lwd = 1.2) +
  scale_color_manual(values = brewer.pal(n = 9, name = "Blues")[c(4, 7)]) +
  ylim(0, 100) +
  ylab("Влажность\nвоздуха, %")
p3 <- pp + 
  geom_bar(data = df2, aes(timestamp, rain), fill = brewer.pal(n = 9, name = "Blues")[4], alpha = 0.5, stat="identity") +
  ylim(0, NA) +
  ylab("Осадки\n(дождь), мм")

# grid.arrange(p1, p2, p3, ncol = 1) # возвращаем ggplot
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))
