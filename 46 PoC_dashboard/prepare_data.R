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

# глобальный исторический прогноз погоды
history_weather_data <- NA
tempDir <- "agritemp"
history_weather_filename <- "weather_history.csv"

# library(ggthemr) # устарело :(
# library(wesanderson)

ggplot_dual_axis <- function(plot1, plot2, which.axis = "x") {
  # исходник взят отсюда: https://gist.github.com/jslefche/e4c0e9f57f0af49fca87
  
  grid.newpage()
  
  # Increase right margin if which.axis == "y"
  if (which.axis == "y")
    plot1 <- plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  
  # Extract gtable
  g1 <- ggplot_gtable(ggplot_build(plot1))
  g2 <- ggplot_gtable(ggplot_build(plot2))
  
  # Overlap the panel of the second plot on that of the first
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Steal axis from second plot and modify
  axis.lab <- ifelse(which.axis == "x", "axis-b", "axis-l")
  ia <- which(g2$layout$name == axis.lab)
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  
  # Switch position of ticks and labels
  if (which.axis == "x"){
    ax$heights = rev(ax$heights)
  } else {
    ax$widths = rev(ax$widths)
  }
  
  ax$grobs <- rev(ax$grobs)
  
  if (which.axis == "x") {
    ax$grobs[[2]]$y <- ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm")
  } else {
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  }
  
  # Modify existing row to be tall enough for axis
  if (which.axis == "x"){
    g$heights[[2]] = g$heights[g2$layout[ia, ]$t]
  }
  
  # Add new row or column for axis label
  if (which.axis == "x") {
    g <- gtable_add_grob(g, ax, 2, 4, 2, 4)
    g <- gtable_add_rows(g, g2$heights[1], 1)
    g <- gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
  } else {
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia,]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
    g <- gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
  }
    
  # Draw it
  grid.draw(g)
}

dual_axis_demo <- function() {
  # взято отсюда: https://gist.github.com/jslefche/e4c0e9f57f0af49fca87
  # требуется
  # library(gtable)
  # library(grid) # для grid.newpage()
  
  
  # Example for x-axis
  # Create fake data.frame
  data.add.x = data.frame(
    y1 = runif(100, 0, 100),
    x1 = runif(100, 0, 100)
  )
  
  # Add second x-axis that scales with first
  data.add.x$x2 = (data.add.x$x1 + 50)^0.75
  
  # Create plots
  plot1.x = qplot(y = y1, x = x1, data = data.add.x)
  plot2.x = qplot(y = y1, x = x2, data = data.add.x)
  
  # Run function
  ggplot_dual_axis(plot1.x, plot2.x, "x")
  
  # Example for y-axis
  
  # Add second y-axis that scales with first
  data.add.x$y2 = (data.add.x$y^0.5) / 500
  
  # Create plots
  plot1.y = qplot(y = y1, x = x1, data = data.add.x)
  plot2.y = qplot(y = y2, x = x1, data = data.add.x)
  
  # Run function
  ggplot_dual_axis(plot1.y, plot2.y, "y")
}

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
  # tick.seq <- seq(as.POSIXct("2016-03-01 23:00:00"), as.POSIXct("2016-03-10 08:32:00"), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  start_time <- round_date(now(), unit = "day") # для синхронных 4-х часовых измерений необходимо привести к 0:00
  tick.seq <- seq(start_time - days(back_days), start_time + days(forward_days), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  # собираем сразу data.frame: время, #сенсора, показание
  # генерируем показатели влажности, допустимый диапазон [0; 100]
  n <- length(tick.seq)
  mydata <- data.frame(name = rep(sensor$name, each = n),
                       lon = rep(sensor$lon, each = n),
                       lat = rep(sensor$lat, each = n),
                       type = "soil_moisture",
                       location = "Капуста 1",
                       #location = "Картофель 1",
                       timestamp = tick.seq, 
                       value = runif(nrow(sensor) * n, 0, 100)) # используем методику дополнения
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
                       temp.min = rnorm(n, 14, 3), # используем методику дополнения
                       pressure = rnorm(n, 750, 30),
                       humidity = runif(n, 10, 100),
                       precipitation = runif(n, 0, 20))

  # максимальная температура должна быть выше минимальной
  # средняя должна быть между максимальной и минимальной
  mydata %<>% mutate(temp.max = temp.min + runif(n, 5, 15)) %>%
    mutate(temp = temp.min + 0.7 * runif(n, 0, temp.max - temp.min))
    
    

  write.table(
    mydata,
    file = ofile,
    sep = ",",
    row.names = FALSE,
    qmethod = "double"
  )
}

plot_field_data <- function(ifile = "./data/appdata_field.csv", back_days = 3) {
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

    # обрежем по глубине анализа
  t.df <- raw.df %>%
    filter(timestamp < lubridate::now()) %>%
    filter(timestamp > lubridate::now() - days(back_days)) %>%
    mutate(timegroup = round_date(timestamp, unit = "hour"))
  
  avg.df <- t.df %>%
    group_by(location, timegroup) %>%
    summarise(value.mean = mean(value), value.sd = sd(value))
  
  object.size(raw.df)
  object.size(avg.df)  
  
  
  # ggplot(avg.df, aes(timegroup, value.mean)) +
  #   geom_point() +
  #   geom_smooth(method="loess", level = 0.99999)
  
  p1 <- ggplot(avg.df, aes(timegroup, value.mean, colour = factor(location))) +
    # ggtitle("График температуры") +
    geom_point() +
    geom_line() +
    #geom_boxplot() +
    ylim(0, NA) +
    theme_solarized() +
    scale_colour_solarized("blue") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 2))
  # theme_solarized(light = TRUE) +
  # scale_colour_solarized("blue")
  # theme_hc() +
  # scale_colour_hc()
  
  
  # http://docs.ggplot2.org/current/geom_boxplot.html
  # You can also use boxplots with continuous x, as long as you supply a grouping variable.
  # http://stackoverflow.com/questions/23433776/change-thickness-of-the-whole-line-geom-boxplot
  p2 <- ggplot(raw.df, aes(timestamp, value)) +
    # geom_point(shape = 1) +
    # geom_line() +
    geom_jitter(width = 0.2) +
    ylim(0, NA) +
    geom_boxplot(aes(group = cut_width(timestamp, 86400/5))) + 
    geom_smooth(method="loess", level = 0.99999)
  
  print(p1)
}

plot_weather_data <- function(ifile = "./data/tweather.csv") {
  
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
  raw.df['timegroup'] <- round_date(raw.df$timestamp, unit = "hour")
  

  # разметим данные на прошлое и будущее. будем использовать для цветовой группировки
  raw.df['time.pos'] <- ifelse(raw.df$timestamp < now(), "PAST", "FUTURE")
  raw.df$temp[raw.df$time.pos == "FUTURE"] <- NA
  
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  plot_palette <- brewer.pal(n = 8, name = "Paired") 
  
  # https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
  p1 <- ggplot(raw.df, aes(timestamp, temp)) +
    # ggtitle("График температуры") +
    # scale_fill_brewer(palette="Set1") +
    scale_fill_brewer(palette = "Paired") +
    geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
    geom_point(shape = 1, size = 3) +
    geom_line(lwd = 1,
              linetype = 'dashed',
              color = "red") +
    theme_igray() +
    theme(legend.position="none")
    
  
  # scale_colour_tableau() +
  # theme_solarized(light = FALSE) +
  # scale_colour_solarized("red")
  
  
  # рисуем влажность в виде geom_bar, это работает для дискретных переменных
  p2 <- ggplot(raw.df, aes(timestamp, precipitation)) +
    # ggtitle("График температуры") +
    # scale_fill_brewer(palette="Set1") +
    scale_fill_brewer(palette = "Paired") +
    geom_bar(fill = plot_palette[7], stat="identity") +
    geom_line(aes(timestamp, temp), lwd = 1,
              linetype = 'dashed',
              color = "red") +
    
    theme_igray() +
    theme(legend.position="none")
  
  
  # geom_area(fill="violet", alpha=.4) +
  # geom_step() +
  # ylim(0, NA)
  # наложим гистограмму с влажностью
  # geom_histogram(breaks = seq(20, 50, by = 2),
  #                col="red",
  #                fill="green",
  #                alpha = .2)
  # theme_solarized() +
  # scale_colour_solarized("blue") +
  # theme(panel.border = element_rect(colour = "black", fill=NA, size = 2))
  # theme_solarized(light = TRUE) +
  # scale_colour_solarized("blue")
  # theme_hc() +
  # scale_colour_hc()
  
  grid.arrange(p1, p2, ncol = 1)
}

test_ordered_dotplot <- function(){
  # тестируем произвольную группировку по текстовому вектору
  # Wakefield: Random Data Set (Part II)
  # https://trinkerrstuff.wordpress.com/2015/04/30/wakefield-random-data-set-part-ii/
  
  # пока сделаем вручную
  n <- 40
  df <- data.frame(x = runif(n, min = 0, max = 6),
                  y = runif(n, min = 0, max = 6),
                  lev = as.character(sample(c('Low', 'Average', 'High', 'Bad'), replace = TRUE, size = n)),
                  stringsAsFactors = FALSE)
  
  df <- data.frame(x = runif(n, min = 0, max = 6),
                  y = runif(n, min = 0, max = 6),
                  val = runif(n, min = -20, max = 120))
  
  # откатегоризируем ручками. значение влажности должно быть в диапазоне [0-100], либо датчик вообще не работает
  
  df$lev <- NA # инициализируем вектор и по умолчанию считаем, что не работает
  df <- within(df, {
    lev[val >= 0 & val <= 33] <- "Low"
    lev[val > 33  & val <= 66] <- "Normal"
    lev[val > 66  & val <= 100] <- "High"
  })  
  
  # при группировке по Lev по умолчанию, порядок следования строк осуществляется по алфавиту
  ggplot(df, aes(x = x, y = y, colour = lev)) + 
    geom_point(size = 4)
  
  # пытаемся изменить группировку
  # http://docs.ggplot2.org/current/aes_group_order.html
  # сделаем из текстовых строк factor и их принудительно отсортируем
  # http://www.ats.ucla.edu/stat/r/modules/factor_variables.htm
  
  # у нас два критерия разделения -- диапазон значений и работоспособность.
  # диапазон измерений -- цвет, работоспособность -- форма
  
  df1 <- df %>%
    # mutate(lev.of = ordered(lev, levels = c('Low', 'Normal', 'High'))) %>%
    mutate(lev.f = factor(lev, levels = c('High', 'Normal', 'Low'))) %>%
    mutate(work.status = !is.na(lev))
  # labels=c("MBB", "MAA", "MCC")
  
  # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
  ggplot(df1 %>% filter(work.status), aes(x = x, y = y, colour = lev.f)) + 
    # http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
    # scale_fill_brewer(palette="Spectral") + 
    # scale_color_manual(values=wes_palette(n=3, name="GrandBudapest")) +
    # scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    scale_color_manual(values=c("royalblue", "palegreen3", "sienna1")) +
    # scale_color_brewer(palette = "Dark2", name = "Влажность\nпочвы") + 
    geom_point(size = 5) + 
    # добавляем нерабочие сенсоры
    geom_point(data = df1 %>% filter(!work.status), size = 5, shape = 21, stroke = 1, colour = 'black', fill = 'gold') +
    geom_point(data = df1 %>% filter(!work.status), size = 5, shape = 13, stroke = 1, colour = 'black')

}

# =================== main ==================

#generate_field_data("./data/tsensors.csv", back_days = 30, forward_days = 30)
#plot_field_data("./data/appdata_field.csv", back_days = 3)
# 
#generate_weather_data("./data/tweather.csv", back_days = 7, forward_days = 7)
#p1 <- plot_weather_data("./data/test_weather.csv")
#p1

test_ordered_dotplot()

# Persistent data storage in Shiny apps
# http://shiny.rstudio.com/articles/persistent-data-storage.html

stop()

# ======== загружаем данные
save_history_weather <- function(data) {
  # data <- t(data)
  # Create a unique file name
  # fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(tempDir, history_weather_filename), 
    row.names = FALSE, quote = TRUE
  )
  # drop_upload('mtcars.csv', dest = "drop_test")
}
load_history_weather <- function(){
  raw.df <- read_delim(
    paste0("./", tempDir, "/", history_weather_filename),
    delim = ",",
    quote = "\"",
    col_names = TRUE,
    locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
    # таймзону, в принципе, можно установить здесь
    # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")),
    progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
  
  raw.df # возвращаем загруженные данные
}
get_current_weather <- function(){
  # получаем данные по текущей погоде с учетом кеширования предыдущих запросов
  if (is.na(history_weather_data)){
    # необходимо подгрузить данные из исторического файла
    history_weather_data <<- load_history_weather()
    print("Подгружаем исторические данные о погоде")
  } 

  # проверим, как давно првоеряли последние показания
  df <- history_weather_data %>%
    filter(timestamp > now() - hours(1)) %>%
    filter(timestamp <= now()) %>%
    filter(timestamp == max(timestamp))
    
  print(df)
  if(nrow(df) != 1){
    # нет записи, походящей критерию, надо запрашивать сайт о погоде
  }

  save_history_weather(history_weather_data)
}

# get_current_weather()

stop()
# как работать с API OpenWeatherMap: http://openweathermap.org/appid
# просмотр погоды по Москве: http://openweathermap.org/city/524901

# Call current weather data for one location By city ID
# http://openweathermap.org/current




url <- "api.openweathermap.org/data/2.5/"   
MoscowID <- '524901'
APPID <- '19deaa2837b6ae0e41e4a140329a1809'
resp <- GET(paste0(url, "weather?id=", MoscowID, "&APPID=", APPID))
if(status_code(resp) == 200){
  r <- content(resp)
  # конструируем вектор
  d <- data.frame(
    timestamp = now(),
    temp = r$main$temp - 273, # пересчитываем из кельвинов в градусы цельсия
    pressure = round(r$main$pressure * 0.75006375541921, 3), # пересчитываем из гектопаскалей (hPa) в мм рт. столба
    humidity = r$main$humidity
    # precipitation = r$main$precipitation
  )
  # history_weather_data  
}
# 



stop()
# ================== повторяем GIS ===========

# ======== загружаем данные
ifile <- "./data/appdata_field.csv"
# подгружаем данные по сенсорам
raw_field.df <- read_delim(ifile, delim = ",", quote = "\"",
                     col_names = TRUE,
                     locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
                     # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")), 
                     progress = interactive()
) # http://barryrowlingson.github.io/hadleyverse/#5

raw_field.df["timegroup"] <- round_date(raw_field.df$timestamp, unit = "hour")
raw_field.df$value <- round(raw_field.df$value, 1)

# для отображения показателей на карте необходимо выбрать для каждого номера сенсора максимально 
# позднее по времени измерение, не превышающее настоящий момент
slicetime <- now()
slicetime <- dmy_hm("29.04.2016 5:00", tz = "Europe/Moscow")
sensors.df <- raw_field.df %>%
  filter(timestamp <= slicetime) %>%
  group_by(name) %>%
  filter(timestamp == max(timestamp)) %>%
  mutate(delta = round(difftime(slicetime, timestamp, unit = "min"), 0)) %>%
  arrange(name) %>%
  ungroup()

# откатегоризируем
sensors.df <- within(sensors.df, {
  level <- NA
  level[value >= 0 & value <= 33] <- "Low"
  level[value > 33  & value <= 66] <- "Average"
  level[value > 66  & value <= 100] <- "High"
})
# и надо сделать reorder
# sensors.df$level <- reorder(sensors.df$level, new.order = c("Low", "Average", "High"))

smap.df <- sensors.df %>%
  select(lon, lat, value) %>%
  rename(val = value)
print(smap.df)

object.size(sensors.df)

stop()
fmap <-
  get_map(
    enc2utf8("Москва, Зоологическая 2"),
    language = "ru-RU",
    # source = "stamen", maptype = "watercolor", 
    # source = "stamen", maptype = "toner-hybrid",
    # source = "stamen", maptype = "toner-2011",
    source = "stamen", maptype = "toner-lite",
    # source = "google", maptype = "terrain",
    # source = "osm", maptype = "terrain-background",
    # source = "google", maptype = "hybrid",
    zoom = 16
  )
ggmap(fmap, extent = "device", legend = "topleft")

# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
plot_palette <- brewer.pal(n = 8, name = "Dark2")

# а теперь попробуем отобразить растром, понимая все потенциальные проблемы
# проблемы хорошо описаны здесь: https://groups.google.com/forum/embed/#!topic/ggplot2/nqzBX22MeAQ
mm3 <- ggmap(fmap, extent = "normal", legend = "topleft") +
  # geom_raster(data = dInterp, aes(x, y, fill = z), alpha = 0.5) +
  # coord_cartesian() +
  # scale_fill_distiller(palette = "Spectral") + #color -- цвет линий
  # stat_contour(data = dInterp, aes(x, y, z = z), bins = 4, color="white", size=0.5) +
  # To use for line and point colors, add
  scale_colour_manual(values = plot_palette) +
  geom_point(data = sensors.df, size = 4, alpha = 0.8, aes(x = lon, y = lat, colour = level)) +
  geom_text(data = sensors.df, aes(lon, lat, label = round(value, digits = 1)), hjust = 0.5, vjust = -1) +
  theme_bw()

mm3

