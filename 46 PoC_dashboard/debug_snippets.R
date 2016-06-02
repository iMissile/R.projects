# генерируем тестовые файлы для отладки интерфейса
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(ggdendro) # для пустой темы
library(lubridate) #load second!
library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(magrittr)
library(httr)
library(ggthemes)
#library(ggmap)
library(RColorBrewer) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
library(scales)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
library(curl)
# library(KernSmooth)
#library(akima)
#library(rdrop2)
# library(rgl)

source("common_funcs.R") # сюда выносим все вычислительные и рисовательные функции

if(FALSE){
# забираем данные по сенсорам в новом формате из репозитория
temp.df <- try({
  read_delim(
    curl("https://github.com/iot-rus/Moscow-Lab/raw/master/result_lab.txt"),
    delim = ";",
    quote = "\"",
    # дата; время; имя; широта; долгота; минимум (0% влажности); максимум (100%); текущие показания
    col_names = c(
      "date",
      "time",
      "rawname",
      "type",
      "lat",
      "lon",
      "yl",
      "xl",
      "yr",
      "xr",
      "voltage",
      "pin"
    ),
    col_types = "Dccc????????",
    locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
    # таймзону, в принципе, можно установить здесь
    progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
})

problems(temp.df)

# проверим только 1-ый элемент класса, поскльку при разных ответах получается разное кол-во элементов
if(class(temp.df)[[1]] != "try-error") {
  # расчитываем необходимые данные
  df <- temp.df %>%
    mutate(value = round(yl + (yr-yl)/(xr-xl) * (voltage - xl), 0), type = factor(type)) %>%
    # откалибруем всплески
    mutate(work.status = (value >= 0 & value <= 100)) %>%
    # получим временную метку
    mutate(timestamp = ymd_hms(paste(date, time), truncated = 3, tz = "Europe/Moscow")) %>%
    # упростим имя сенсора
    mutate(label = gsub(".*:", "", rawname, perl = TRUE)) %>%
    # и разделим на имя и адрес
    separate(label, c('ipv6', 'name'), sep = '-', remove = TRUE) %>%
    select(timestamp, name, type, value, voltage, work.status, lat, lon, pin) %>%
    mutate(location = "Moscow Lab") 
  
  #flog.info("Sensors data from GitHub recieved. Last records:")
  #flog.info(capture.output(print(head(arrange(df, desc(timestamp)), n = 4))))

  # откатегоризируем значения вольтажа для сенсора влажности
  # df$level <- NA
  # df <- df %>%
  #   mutate(level = ifelse(type == 'MOISTURE' & voltage > 2450000, "DRY+", NA))
  # 
  # # или так
  # df <- within(df, {
  #      level <- NA
  #      level[type == 'MOISTURE' & voltage > 2450000] <- "DRY+"
  #    })  
    
  # или даже так
  levs <- list(step = c(0, 1000, 2270, 2330, 2390, 2450, 4000) * 1000, 
            category = c('WET++', 'WET+', 'WET', 'NORM', 'DRY', 'DRY+', 'DRY++'))
  
  # отсечем весь шлак
  df <- df %>%
    filter(voltage >= levs$step[3] & voltage <= levs$step[-2])
  
  df$level <- NA
  for (i in 1:(length(levs$step) - 1)){
    print(paste0("i = ", i, " ", levs$category[i]))
    df$level[df$type == 'MOISTURE' & 
               df$voltage > levs$step[i] &
               df$voltage <= levs$step[i+1]] <- levs$category[i]
  }
} else {
  df <- NA # в противном случае мы сигнализируем о невозможности обновить данные
  flog.error("GitHub connection error")
}

}

df <- load_github_field2_data()

# выведем сырые данные по влажности и температуре
p1 <- ggplot(df, aes(timestamp, value, colour = name)) +
  geom_line() +
  geom_point(shape = 1) +
  scale_y_sqrt(limits = c(10, 30)) +
  facet_grid(type ~ .)
  
p1


p2 <- ggplot(df %>% filter(type == 'MOISTURE'), aes(timestamp, voltage, colour = name)) +
  geom_line() +
  geom_point(shape = 1)

p2



stop()
# # получаем исторические данные по погоде из репозитория Гарика --------------------------------------------------------
# # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
# req <- curl_fetch_memory("https://raw.githubusercontent.com/iot-rus/Moscow-Lab/master/weather.txt")
# wrecs <- rawToChar(req$content) # weather history
# # wh_json <- gsub('\\\"', "'", txt, perl = TRUE) 
# # заменим концы строк на , и добавим шапочку и окончание для формирования семантически правильного json
# # последнюю ',' надо удалить, может такое встретиться (перевод строки)
# tmp <- paste0('{"res":[', gsub("\\n", ",\n", wrecs, perl = TRUE), ']}')
# wh_json <- gsub("},\n]}", "}]}", tmp)
# # t <- cat(wh_json)
# # write(wh_json, file="./export/wh_json.txt")
# data <- fromJSON(wh_json)
# 
# whist.df <- data$res$main
# whist.df$timestamp <- data$res$dt
# # поскольку историю мы сохраняем сами из данных текущих запросов, то
# # rain$3h -- Rain volume for the last 3 hours (http://openweathermap.org/current#parameter)
# whist.df$rain3h <- data$res$rain[['3h']]
# whist.df$human_time <- as.POSIXct(whist.df$timestamp, origin='1970-01-01')

weather.df <- prepare_raw_weather_data()

# считаем осадки за сутки ------------------------------
# полагаем, что идентичность выпавших осадков с точностью до третьего знака просто означает дублирование показаний!!!!
df0 <- data.frame(timestamp = weather.df$timestamp, rain3h = weather.df$rain3h) %>%
  filter(!is.na(rain3h)) %>% # записи без дождя нас вообще не интересуют
  distinct() %>% # полностью дублирующиеся записи также неинтересны
  mutate(date = lubridate::date(timestamp)) %>%
  group_by(date, rain3h) %>% # собираем агрегаты по суткам, а потом по повторяющимся значениям, 
  # может быть погрешность по переходам через сутки, но при группировке по значениям можем случайно объединить данных с разных дат
  # в каждой группе посчитаем временную протяженность события
  arrange(timestamp) %>%
  mutate (dtime = as.numeric(difftime(timestamp, min(timestamp), unit = "min")))
  
# теперь мы можем проверить, чтобы максимальное значение в группе не превышало 180 мин (3 часа)
# поглядел на данные, таких групп нет за месяц не нашел, решил пока для простоты забить
df1 <- df0 %>% 
  # в каждой группе выберем значение с минимальным временем измерения
  filter(timestamp == min(timestamp)) %>% # см. допущение об идентичности показаний
  ungroup() %>%
  arrange(timestamp)

# а теперь посчитаем агрегаты по суткам
df2 <- df1 %>%
  select(-dtime) %>%
  group_by(date) %>%
  summarise(rain = sum(rain3h)) %>% # пытаемся высчитать агрегат за сутки
  ungroup %>%
  arrange(date)
  
  

# # попробуем на каждое измерение добавить строчку с 0-ми осадками на 3 часа назад. Посмотрим как будет выглядеть
# df2 <- bind_rows(select(df0, -date), data.frame(timestamp = df0$timestamp - hours(3), rain3h = 0)) %>%
#   arrange(timestamp)
# # получается фигня
# 
# # посчитаем дельту во времени между измерениями
# # df3 <- data.frame(df0$timestamp, dt = c(NA, difftime(tail(df0$timestamp, -1), head(df0$timestamp, -1), unit = "min")))
# df0$dt <- c(3*60, as.numeric(difftime(tail(df0$timestamp, -1), head(df0$timestamp, -1), unit = "min")))
# # df0$dt <- c(3*60*60, (tail(df0$timestamp,-1) - head(df0$timestamp,-1))) # полагаем, что предыдущее измерение было более 3-х часов назад
# 
# 
# # попробуем по другому
# df <- df0
#   
#   
# # посчитаем дельту в осадках
# # # http://stackoverflow.com/questions/16212097/r-how-can-i-calculate-difference-between-rows-in-a-data-frame
# df0$dt <- c(3*60*60, (tail(df0$timestamp,-1) - head(df0$timestamp,-1))) # полагаем, что предыдущее измерение было более 3-х часов назад
# df0$dr <- c(head(df0$rain3h, 1), (tail(df0$rain3h,-1) - head(df0$rain3h,-1)))
# 
# # устраним все записи с нулевой дельтой (это просто повторы предыдущих событий, дождь может уже и кончился))
# df <- filter(df0, dr != 0)
# # отбросим записи, которые следуют чаще, чем через час
#   
# myfun <- function(x){
#   res <- sum(x$rain3h, na.rm = TRUE)
#   print(paste0("==============================="))
#   print(x)
#   print(paste0("----- Sum:", res, " --------"))
#   res
#   }
# 
# df1 <- df %>%
#   mutate(date = lubridate::date(timestamp)) %>%
#   group_by(date) %>% # собираем агрегаты по суткам
#   summarise(rain = myfun(data.frame(timestamp = timestamp, rain3h = rain3h))) # пытаемся высчитать агрегат за сутки
#   

ggplot(df2, aes(date, rain)) +
  geom_point()


stop()

# ====================================
url <- "api.openweathermap.org/data/2.5/"   
MoscowID <- '524901'
APPID <- '19deaa2837b6ae0e41e4a140329a1809'
resp <- GET(paste0(url, "weather?id=", MoscowID, "&APPID=", APPID))
if(status_code(resp) == 200){
  r <- content(resp)
  # конструируем вектор
  d <- data.frame(
    # timestamp = now(),
    timestamp = as.POSIXct(r$dt, origin='1970-01-01'),
    temp = round(r$main$temp - 273.15, 1), # пересчитываем из кельвинов в градусы цельсия
    pressure = round(r$main$pressure * 0.75006375541921, 0), # пересчитываем из гектопаскалей (hPa) в мм рт. столба
    humidity = round(r$main$humidity, 0)
    # precipitation = r$main$precipitation
  )
}

df <- data.frame(x = c(0, 1), y = c(0, 1))

windowsFonts(verdana = "TT Verdana")
windowsFonts(geinspira = "GE Inspira")
windowsFonts(corbel = "Corbel")
p <- ggplot(df, aes(x, y)) + 
  geom_point() +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1), fill = "peachpuff") +
  geom_text(aes(.5, .8), label = paste0(d$temp, " C"), size = 40, color="blue", family = "verdana") +
  geom_text(aes(.5, .5), label = paste0(d$pressure, " мм"), size = 16, color="blue", family = "verdana") +
  geom_text(aes(.5, .3), label = paste0(d$humidity, " %"), size = 16, color="blue", family = "verdana") +
  geom_text(aes(.5, .1), label = paste0(d$timestamp), size = 7, color="blue", family = "verdana") +
  theme_dendro() # совершенно пустая тема


print(p)





stop()


ifile <- "./data/appdata_field.csv"
back_days <- 3
# отобразим за указанный диапазон график: среднее, стандартное отклонение в виде серой области, выбросы

# подгружаем данные по сенсорам
#x <- read.csv( curl("https://github.com/iot-rus/Moscow-Lab/raw/master/result_moisture.txt") )
temp.df <- try({
  read_delim(
    curl("https://github.com/iot-rus/Moscow-Lab/raw/master/result_moisture.txt"),
    delim = ";",
    quote = "\"",
    # дата; время; имя; широта; долгота; минимум (0% влажности); максимум (100%); текущие показания
    col_names = c(
      "date",
      "time",
      "name",
      "lat",
      "lon",
      "calibration_0",
      "calibration_100",
      "voltage"
    ),
    locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
    # таймзону, в принципе, можно установить здесь
    progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
})


if(class(temp.df) != "try-error") {
  # расчитываем необходимые данные
  raw.df <- temp.df %>%
    mutate(value = 100 / (calibration_100 - calibration_0) * (voltage - calibration_0)) %>%
    # откалибруем всплески
    mutate(work.status = (value >= 0 & value <= 100)) %>%
    # получим временную метку
    mutate(timestamp = ymd_hm(paste(date, time), tz = "Europe/Moscow")) %>%
    select(-calibration_0, -calibration_100, -voltage)
}
# в противном случае мы просто оставляем данные неизменными

# http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
p <- ggplot(raw.df %>% filter(work.status), aes(x = timestamp, y = value, colour = name)) + 
  # http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
  # scale_fill_brewer(palette="Spectral") + 
  # scale_color_manual(values=wes_palette(n=3, name="GrandBudapest")) +
  # scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  # рисуем разрешенный диапазон
  
  geom_ribbon(aes(ymin = 70, ymax = 90), fill = "chartreuse") +
  geom_line(lwd = 2) +
  geom_point(shape = 19, size = 3) +
  geom_hline(yintercept = c(70, 90), lwd = 1.2, linetype = 'dashed') +
  
  scale_x_datetime(labels = date_format(format = "%d.%m%n%H:%M", tz = "Europe/Moscow"),
                   breaks = date_breaks('2 hour'), 
                   minor_breaks = date_breaks('1 hour')) +
  # добавляем нерабочие сенсоры
  #geom_point(data = raw.df %>% filter(!work.status), size = 3, shape = 21, stroke = 0, colour = 'red', fill = 'yellow') +
  #geom_point(data = raw.df %>% filter(!work.status), size = 3, shape = 13, stroke = 1.1, colour = 'red')

  theme_igray() + 
  scale_colour_tableau("colorblind10", name = "Влажность\nпочвы") +
  # scale_color_brewer(palette = "Set2", name = "Влажность\nпочвы") +
  ylim(0, 100) +
  xlab("Время и дата измерения") +
  ylab("Влажность почвы, %") +
  # theme_solarized() +
  # scale_colour_solarized("blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.text.y = element_text(angle = 0))




p




stop()






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
  summarise(value.mean = mean(value), value.sd = sd(value)) %>%
  ungroup() # очистили группировки

object.size(raw.df)
object.size(avg.df)


# ggplot(avg.df, aes(timegroup, value.mean)) +
#   geom_point() +
#   geom_smooth(method="loess", level = 0.99999)

p1 <-
  ggplot(avg.df, aes(timegroup, value.mean, colour = factor(location))) +
  # ggtitle("График температуры") +
  geom_point() +
  geom_line() +
  #geom_boxplot() +
  ylim(0, NA) +
  theme_solarized() +
  scale_colour_solarized("blue") +
  theme(panel.border = element_rect(
    colour = "black",
    fill = NA,
    size = 2
  ))
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
  geom_boxplot(aes(group = cut_width(timestamp, 86400 / 5))) +
  geom_smooth(method = "loess", level = 0.99999)


p3 <-
  ggplot(t.df, aes(factor(timegroup), value)) +
  # ggtitle("График температуры") +
  # geom_point() +
  geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 5, alpha = 0.5) +
  # почему надо писать group = 1: http://stackoverflow.com/questions/16350720/using-geom-line-with-x-axis-being-factors
  # geom_line(data = avg.df, aes(x = factor(timegroup), y = value.mean, group=1)) +
  geom_smooth(data = avg.df, aes(x = factor(timegroup), y = value.mean, group=1), size = 1.5, method = "loess", se = FALSE) +
  ylim(0, NA)



# проверим округление дат и подсчет интервалов
round_date(lubridate::dmy_hm("11.05.2016 6:14"), unit = "day")
round_date(lubridate::dmy_hm("11.05.2016 20:14"), unit = "day")
t <- abs(as.numeric(difftime(Sys.time(), lubridate::dmy_hm("11.05.2016 6:14"), unit = "min")))

str(t)



plot_palette <- brewer.pal(n = 8, name = "Paired") 

# http://stackoverflow.com/questions/20326946/how-to-put-ggplot2-ticks-labels-between-dollars
my_date_format <- function(format = "%d.%m, %H:%M", tz = "Europe/Moscow") {
  # делаем хитрую функцию условного форматирования
  # для начала суток указываем дату, для остальных меток, только время
  
  function(x){
    # на вход поступает вектор дат, на выходе надо выдать вектор форматов
    # оценим расстояние до границы суток
    # dput(x)
    # dt <- abs(as.numeric(difftime(x, round_date(x), unit = "min")))
    # dput(dt)
    
    labels <- lapply(x, function(el) {
      print(paste0("Element:", el))
      dt <-
        abs(as.numeric(difftime(el, round_date(el, unit = "day"), unit = "min")))
      str(dt)
      if (is.na(dt)) {
        ret <- NA
      }
      else {
        if (dt < 130) {
          # допустим разброс в 30 минут
          ret <- format(el, "%d.%m,\n%H:%M", tz = tz)
        } else {
          ret <- format(el, "%H:%M", tz = tz)
        }
      }
      ret
    })

    labels
  }
}

p4 <- ggplot(avg.df, aes(timegroup, value.mean)) +
  ggtitle("Влажность почвы") +
  geom_line(lwd = 2, colour = "blue") +
  # рисуем разрешенный диапазон
  geom_ribbon(aes(ymin = 70, ymax = 90), fill = plot_palette[3]) +
  geom_hline(yintercept = 70) +
  geom_hline(yintercept = 90) +
  geom_ribbon(
    aes(ymin = value.mean - value.sd, ymax = value.mean + value.sd),
    fill = plot_palette[1],
    alpha = 0.8
  ) +
  geom_point() +
  #geom_smooth(size = 1.5, method = "loess", se = FALSE) +
  scale_x_datetime(labels = date_format(format = "%d.%m,\n%H:%M", tz = "Europe/Moscow"),
                   breaks = date_breaks('4 hours')) +
                   # minor_breaks = date_breaks('4 hours')) +
  ylim(0, NA) +
  xlab("Время и дата измерения") +
  ylab("Влажность почвы, %") +
  theme_solarized() +
  scale_colour_solarized("blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4
