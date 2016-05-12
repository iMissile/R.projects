# генерируем тестовые файлы для отладки интерфейса
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(readr)
library(jsonlite)
library(magrittr)
#library(httr)
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


ifile <- "./data/appdata_field.csv"
back_days <- 3
# отобразим за указанный диапазон график: среднее, стандартное отклонение в виде серой области, выбросы

# подгружаем данные по сенсорам
#x <- read.csv( curl("https://github.com/iot-rus/Moscow-Lab/raw/master/result.txt") )
temp.df <- try({
  read_delim(
    curl("https://github.com/iot-rus/Moscow-Lab/raw/master/result.txt"),
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
