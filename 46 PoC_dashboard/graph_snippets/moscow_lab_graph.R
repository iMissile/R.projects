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
#library(KernSmooth)
#library(akima)
#library(rdrop2)
#library(rgl)


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
      mutate(value = 100 / (calibration_100 - calibration_0) * (voltage - calibration_0)) %>%
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

