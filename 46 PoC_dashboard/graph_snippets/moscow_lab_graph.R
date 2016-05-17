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
library(wesanderson) # https://github.com/karthik/wesanderson
#library(ggmap)
library(RColorBrewer) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
library(scales)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
library(curl)
#library(KernSmooth)
#library(akima)
#library(rdrop2)
#library(rgl)



test_timegroup <- function() {
  dd <- dmy_hm("12-05-2016 1:30", tz = "Europe/Moscow")
  dd
  time.bin <- 3
  dd + minutes(time.bin * 60) / 2
  n <- floor(hour(dd + minutes(time.bin * 60) / 2) / time.bin)
  n
  
  floor_date(dd + minutes(time.bin * 60) / 2, unit = "day") + hours(n * time.bin)
}


hgroup.enum <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток +-1/2 интервала, к точке измерения. 
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  tick_time <- date + minutes(time.bin * 60)/2 # сдвигаем на пол интервала вперед
  n <- floor(hour(tick_time) / time.bin)
  floor_date(tick_time, unit = "day") + hours(n * time.bin)
}

load_github_field_data <- function() {
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
    df <- temp.df %>%
      mutate(value = round(100 / (calibration_100 - calibration_0) * (voltage - calibration_0), 0)) %>%
      # откалибруем всплески
      mutate(work.status = (value >= 0 & value <= 100)) %>%
      # получим временную метку
      mutate(timestamp = ymd_hm(paste(date, time), tz = "Europe/Moscow")) %>%
      # упростим имя сенсора
      mutate(name = gsub(".*:", "", name, perl = TRUE)) %>%
      mutate(location = "Moscow Lab") %>%
      select(-calibration_0, -calibration_100, -voltage, -date, -time)
  } else {
    df <- NA # в противном случае мы сигнализируем о невозможности обновить данные
    }
  df
}

# main() ===================================================

df <- load_github_field_data()
if (!is.na(df)) { raw.df <- df}
# .Last.value

# проведем усреднение по временным группам, если измерения проводились несколько раз в течение этого времени
# усредняем только по рабочим датчикам

# сгруппируем по временным интервалам
raw.df <- raw.df %>%
  mutate(timegroup = hgroup.enum(timestamp, time.bin = 4))
  
  
avg.df <- raw.df %>%
  filter(work.status) %>%
  group_by(location, name, timegroup) %>%
  summarise(value.mean = mean(value), value.sd = sd(value)) %>%
  ungroup() # очистили группировки

plot_palette <- brewer.pal(n = 5, name = "Blues")
plot_palette <- wes_palette(name="Moonrise2") # https://github.com/karthik/wesanderson

# -----------------------------------------------------------
# http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
p1 <- ggplot(raw.df %>% filter(work.status), aes(x = timegroup, y = value, colour = name)) + 
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
                   breaks = date_breaks('4 hour') 
                   # minor_breaks = date_breaks('1 hour')
                   ) +
  # добавляем нерабочие сенсоры
  geom_point(data = raw.df %>% filter(!work.status), size = 3, shape = 21, stroke = 0, colour = 'red', fill = 'yellow') +
  geom_point(data = raw.df %>% filter(!work.status), size = 3, shape = 13, stroke = 1.1, colour = 'red') +

  theme_igray() + 
  scale_colour_tableau("colorblind10", name = "Влажность\nпочвы") +
  # scale_color_brewer(palette = "Set2", name = "Влажность\nпочвы") +
  # ylim(0, 100) +
  xlab("Время и дата измерения") +
  ylab("Влажность почвы, %") +
  # theme_solarized() +
  # scale_colour_solarized("blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.text.y = element_text(angle = 0))

# -----------------------------------------------------------
# http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
p2 <- ggplot(avg.df, aes(x = timegroup, y = value.mean, colour = name)) + 
  # http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
  # scale_fill_brewer(palette="Dark2") +
  # scale_color_brewer(palette="Dark2") + 
  scale_color_manual(values = plot_palette) +
  scale_fill_manual(values = plot_palette) +
  #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  # рисуем разрешенный диапазон
  
  geom_ribbon(aes(ymin = 70, ymax = 90), fill = "darkseagreen1") +
  geom_ribbon(aes(ymin = 70, ymax = 90), fill = "mediumaquamarine") +
  geom_ribbon(
    aes(ymin = value.mean - value.sd, ymax = value.mean + value.sd, fill = name),
    alpha = 0.3
  ) +
  geom_line(lwd = 1.5) +
  geom_point(data = raw.df, aes(x = timestamp, y = value), shape = 1, size = 2) +
  geom_hline(yintercept = c(70, 90), lwd = 1.2, linetype = 'dashed') +
  geom_point(shape = 19, size = 3) +
  scale_x_datetime(labels = date_format(format = "%d.%m%n%H:%M", tz = "Europe/Moscow"),
                   breaks = date_breaks('8 hour') 
                   # minor_breaks = date_breaks('1 hour')
  ) +
  # добавляем нерабочие сенсоры
  # geom_point(data = raw.df %>% filter(!work.status), aes(x = timegroup, y = value), 
  #            size = 3, shape = 21, stroke = 0, colour = 'red', fill = 'yellow') +
  # geom_point(data = raw.df %>% filter(!work.status), aes(x = timegroup, y = value), 
  #            size = 3, shape = 13, stroke = 1.1, colour = 'red') +
  
  theme_igray() + 
  # scale_colour_tableau("colorblind10", name = "Влажность\nпочвы") +
  # scale_color_brewer(palette = "Set2", name = "Влажность\nпочвы") +
  # ylim(0, 100) +
  xlab("Время и дата измерения") +
  ylab("Влажность почвы, %") +
  # theme_solarized() +
  # scale_colour_solarized("blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  theme(axis.text.y = element_text(angle = 0))

benchplot(p2)

# и сохраним в файл
# http://stackoverflow.com/questions/25550711/convert-data-frame-to-json
x <- jsonlite::toJSON(avg.df, pretty = TRUE)
# x <- serializeJSON(avg.df, digits = 3, pretty = TRUE)
cat(x)
write(x, file="./export/avg_df.json")

