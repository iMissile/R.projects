# генерируем тестовые файлы для отладки интерфейса
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(readr)
library(ggthemes)
library(ggmap)
# library(ggthemr) # устарело :(
# library(wesanderson)


generate_field_data <- function(ofile = "tsensors.csv", back_days = 7, forward_days = 7) {
  # данные по расположению сенсоров
  sensor <- data.frame(
    c(1, 37.578691607470724, 55.766160765720493),
    c(2, 37.57990362015564, 55.766504551149772),
    c(3, 37.580025839922193, 55.765874275547077),
    c(4, 37.577632369493983, 55.764819973580607),
    c(5, 37.581431367237478, 55.764384492709446),
    c(6, 37.579109191673091, 55.765158040903287),
    c(7, 37.58182858147876, 55.766160765720493),
    c(8, 37.580586013852198, 55.767008764295831),
    c(9, 37.579862880233463, 55.764109449653162),
    c(10, 37.577856439065989, 55.766567578149633),
    c(11, 37.576776831128157, 55.76609773806296)
  )
  # sensor <- t(sensor) #транспонируем
  # sensor <- tFrame(sensor) #транспонируем
  sensor <- as.data.frame(t(sensor))
  rownames(sensor) <- NULL
  colnames(sensor) <- c("name", "lon", "lat") # http://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame-in-r
  
  #sensor <- as.data.frame(t(sensor)) #транспонируем
  # dimnames(sensor) <- NULL
  
  # генерируем данные по показаниям сенсоров (почасовая раскладка)
  # смотрим за последнюю неделю
  # tick.seq <- seq(as.POSIXct("2016-03-01 23:00:00"), as.POSIXct("2016-03-10 08:32:00"), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  tick.seq <- seq(now() - days(back_days), now() + days(forward_days), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  # собираем сразу data.frame: время, #сенсора, показание
  mydata <- data.frame(name = rep(sensor$name, each = length(tick.seq)),
                       type = "Temp",
                       location = "Капуста 1",
                       #location = "Картофель 1",
                       timestamp = tick.seq, 
                       value = rnorm(nrow(sensor)*length(tick.seq), 15, 1)) # используем методику дополнения
  # rnorm(1000, 3, .25) # Generates 1000 numbers from a normal with mean 3 and sd=.25
  
  # для соотв реалиям сделаем разброс во времени измерения
  mydata$timestamp <- mydata$timestamp + runif(nrow(mydata), min = -5*60, max = 5*60)
  mydata$value <- round(mydata$value + 0.2*sin(as.numeric(mydata$timestamp)/86400*(0.1*pi)), 1)

  qplot(timestamp, value, data = mydata)
  
  write.table(
    mydata,
    file = ofile,
    sep = ",",
    row.names = FALSE,
    qmethod = "double"
  )
}

generate_weather_data <- function(ofile = "tweather.csv", back_days = 7, forward_days = 7) {
  # генерируем погодные даты
  
  tick.seq <- seq(now() - days(back_days), now() + days(forward_days), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r

  # для подготовки данных 
  # собираем сразу data.frame: время, #сенсора, показание
  n <- length(tick.seq)
  mydata <- data.frame(timestamp = tick.seq,
                       temp.min = rnorm(n, 12, 5), # используем методику дополнения
                       temp.max = rnorm(n, 25, 5),
                       temp = rnorm(n, 17, 5),
                       pressure = rnorm(n, 750, 30),
                       humidity = runif(n, 10, 100),
                       precipitation = runif(n, 0, 20))
  write.table(
    mydata,
    file = ofile,
    sep = ",",
    row.names = FALSE,
    qmethod = "double"
  )
}

plot_field_data <- function(ifile = ".\\data\\test_data.csv") {
  # отобразим за указанный диапазон график: среднее, стандартное отклонение в виде серой области, выбросы

  # подгружаем данные по сенсорам
  raw.df <- read_delim(ifile, delim = ",", quote = "\"",
                       col_names = TRUE,
                       locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
                       # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")), 
                       progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
  
  # сформируем часовые группы и посчитаем среднее по ансамблю сенсоров за каждую часовую группу
  # raw.df["time.group"] <- lapply(raw.df$timestamp, function(x){round(x, units="hours")})
  # t <- raw.df$timestamp
  # object.size(t)
  
  # m <- lapply(raw.df$timestamp, function(x){round(x, units="hours")}) # так из 130 кб получаем 35 Мб, надо использовать round_date {lubridate}
  # m <- lapply(t, function(x){round_date(x, unit = "hour")}) # тут получаем 8Мб !!!
  # m <- round_date(t, unit = "hour") # самый быстрый и компактный вариант
  # object.size(m)
  # object.size(m[[2]])
  raw.df["timegroup"] <- round_date(raw.df$timestamp, unit = "hour")
  
  # t.df <- raw.df %>%
  #   filter(timegroup < lubridate::now()) %>%
  #   filter(timegroup > lubridate::now() - days(3))
  
  avg.df <- raw.df %>%
    group_by(location, timegroup) %>%
    summarise(value.mean = mean(value), value.sd = sd(value))
  
  object.size(raw.df)
  object.size(avg.df)  
  
  
  # ggplot(avg.df, aes(timegroup, value.mean)) +
  #   geom_point() +
  #   geom_smooth(method="loess", level = 0.99999)
  
  ggplot(avg.df, aes(timegroup, value.mean, colour = factor(location))) +
    # ggtitle("График температуры") +
    geom_point() +
    geom_line() +
    ylim(0, NA) +
    theme_solarized() +
    scale_colour_solarized("blue") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size = 2))
  # theme_solarized(light = TRUE) +
  # scale_colour_solarized("blue")
  # theme_hc() +
  # scale_colour_hc()
  
  
  # http://docs.ggplot2.org/current/geom_boxplot.html
  # You can also use boxplots with continuous x, as long as you supply a grouping variable.
  # http://stackoverflow.com/questions/23433776/change-thickness-of-the-whole-line-geom-boxplot
  ggplot(raw.df, aes(timestamp, value)) +
    # geom_point(shape = 1) +
    # geom_line() +
    geom_jitter(width = 0.2) +
    ylim(0, NA) +
    geom_boxplot(aes(group = cut_width(timestamp, 86400/5))) + 
    geom_smooth(method="loess", level = 0.99999)
}

plot_weather_data <- function(ifile = ".\\data\\tweather.csv") {
  
  raw.df <- read_delim(
    ifile,
    delim = ",",
    quote = "\"",
    col_names = TRUE,
    locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
    # таймзону, в принципе, можно установить здесь
    # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")),
    progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
  
  # сформируем часовые группы и посчитаем среднее по ансамблю сенсоров за каждую часовую группу
  raw.df["timegroup"] <- round_date(raw.df$timestamp, unit = "hour")
  

  # https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
  ggplot(raw.df, aes(timestamp, temp)) +
    # ggtitle("График температуры") +
    geom_point() +
    geom_line() +
    ylim(0, NA) +
    theme_solarized() +
    scale_colour_solarized("blue") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size = 2))
  # theme_solarized(light = TRUE) +
  # scale_colour_solarized("blue")
  # theme_hc() +
  # scale_colour_hc()
}


# =================== main ==================

# ofile <- ".\\data\\tsensors.csv"
# generate_field_data(ofile, back_days = 7, forward_days = 0)
# plot_field_data(".\\data\\test_data.csv")
# 
# generate_weather_data(".\\data\\tweather.csv", back_days = 7, forward_days = 3)
plot_weather_data(".\\data\\tweather.csv")

stop()


# ======================
getMap <- get_map(
  enc2utf8("Москва, Зоологическая 2"),
  language = "ru-RU",
  # source = "stamen",
  # maptype = "watercolor", 
  maptype = "terrain",
  zoom = 16
)

ggmap(getMap, extent="panel")

