
load_field_data <- function() {
  ifile <- "..\\.\\data\\appdata_field.csv"
  # подгружаем данные по сенсорам
  raw.df <- read_delim(ifile, delim = ",", quote = "\"",
                       col_names = TRUE,
                       locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
                       # col_types = list(date = col_datetime(format = "%d.%m.%Y %H:%M")), 
                       progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5
  
  raw.df["timegroup"] <- round_date(raw.df$timestamp, unit = "hour")
  raw.df$value <- round(raw.df$value, 1)
  
  raw.df # возвращаем загруженные данные
}

load_weather_data <- function() {
  ifile <- "..\\.\\data\\appdata_weather.csv"
  
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
  raw.df['timegroup'] <-
    round_date(raw.df$timestamp, unit = "hour")
  
  raw.df # возвращаем загруженные данные
}

plot_ts_data <- function(raw.df) {

  avg.df <- raw.df %>%
    group_by(location, timegroup) %>%
    summarise(value.mean = mean(value), value.sd = sd(value))
  
  p <- ggplot(avg.df, aes(timegroup, value.mean, colour = factor(location))) +
    # ggtitle("График температуры") +
    geom_point() +
    geom_line() +
    ylim(0, NA) +
    theme_solarized() +
    scale_colour_solarized("blue") +
    theme(panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 2
    ))
  
  p # возвращаем ggplot
}

plot_weather_data <- function(raw.df) {
  # разметим данные на прошлое и будущее. будем использовать для цветовой группировки
  raw.df['time.pos'] <-
    ifelse(raw.df$timestamp < now(), "PAST", "FUTURE")
  raw.df$temp[raw.df$time.pos == "FUTURE"] <- NA
  
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  plot_palette <- brewer.pal(n = 8, name = "Paired") 
  
  # https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
  p1 <- ggplot(raw.df, aes(timestamp, temp)) +
    # ggtitle("График температуры") +
    # scale_fill_brewer(palette="Set1") +
    scale_fill_brewer(palette = "Paired") +
    geom_ribbon(aes(
      ymin = temp.min,
      ymax = temp.max,
      fill = time.pos
    ), alpha = 0.5) +
    geom_point(shape = 1, size = 3) +
    geom_line(lwd = 1,
              linetype = 'dashed',
              color = "red") +
    theme_igray() +
    theme(legend.position="none")
  
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
  
  grid.arrange(p1, p2, ncol = 1) # возвращаем ggplot
}

draw_field_ggmap <- function() {
  fmap <-
    get_map(
      # enc2utf8("Москва, Зоологическая 2"),
      "Москва, Зоологическая 2", # надо понимать из какой кодировки грузим
      language = "ru-RU",
      # source = "stamen", maptype = "watercolor", 
      # source = "stamen", maptype = "toner-hybrid",
      # source = "stamen", maptype = "toner-lite",
      source = "google", maptype = "terrain",
      # source = "osm", maptype = "terrain-background",
      # source = "google", maptype = "hybrid",
      # source = "stamen", maptype = "toner-2011",
      zoom = 16
    )
  p <- ggmap(fmap, extent = "panel")
  
  p # возвращаем ggpmap!
}



