
# http://stackoverflow.com/questions/20326946/how-to-put-ggplot2-ticks-labels-between-dollars
my_date_format <- function(format = "%d %b", tz = "Europe/Moscow") {
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
      # str(dt)
      if (is.na(dt)) {
        ret <- NA
      }
      else {
        if (dt < 130) {
          # допустим разброс в 30 минут
          # ret <- format(el, "%d.%m\n%H:%M", tz = tz)
          ret <- format(el, "%d %h    ", tz = tz)
        } else {
          ret <- format(el, "%H:%M", tz = tz)
        }
      }
      ret
    })
    
    labels
  }
}

load_field_data <- function() {
  ifile <- ".././data/appdata_field.csv"
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

load_weather_data <- function() {
  ifile <- ".././data/appdata_weather.csv"
  
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

plot_average_ts_data <- function(raw.df, ddepth = 1) {
  
  df <- raw.df %>%
    filter(timegroup < lubridate::now()) %>%
    filter(timegroup > floor_date(lubridate::now() - days(ddepth), unit = "day"))
  
  # print(raw.df)
  plot_palette <- brewer.pal(n = 5, name = "Blues")
  
  avg.df <- df %>%
    group_by(location, timegroup) %>%
    summarise(value.mean = mean(value), value.sd = sd(value)) %>%
    ungroup() # очистили группировки
  
  # Что делать, если метки на графике надо расставить по фиксированным местам?
  # [how to fix x-axis and y-axis scale](http://stackoverflow.com/questions/30799845/how-to-fix-x-axis-and-y-axis-scale)
  man.lims <- c(min(avg.df$timegroup), max(avg.df$timegroup))
  man.breaks <- seq(from = man.lims[1], to = man.lims[2], by = "4 hours")
  
  p <- ggplot(avg.df, aes(timegroup, value.mean)) +
    # ggtitle("Влажность почвы") +
    # рисуем разрешенный диапазон
    geom_ribbon(aes(ymin = 70, ymax = 90), fill = "chartreuse") +
    geom_ribbon(
      aes(ymin = value.mean - value.sd, ymax = value.mean + value.sd),
      fill = plot_palette[3],
      alpha = 0.8
    ) +
    geom_line(lwd = 2, colour = plot_palette[4]) +
    geom_point(shape = 19,
               size = 5,
               colour = plot_palette[5]) +
    geom_hline(yintercept = c(70, 90),
               lwd = 1.2,
               linetype = 'dashed') +
    # geom_hline(yintercept = 90) +
    #geom_smooth(size = 1.5, method = "loess", se = FALSE) +
    scale_x_datetime(
      labels = date_format(format = "%d.%m\n%H:%M", tz = "Europe/Moscow"),
      breaks = man.breaks,
      limits = man.lims
    ) +
    # scale_x_datetime(labels = date_format(format = "%d.%m %H:%M", tz = "Europe/Moscow"),
    # breaks = date_breaks('4 hours')) +
    # minor_breaks = date_breaks('4 hours')) +
    theme_igray() +
    scale_colour_tableau("colorblind10") +
    ylim(0, NA) +
    xlab(enc2utf8("Время и дата измерения")) +
    ylab(enc2utf8("Влажность почвы, %")) +
    # theme_solarized() +
    # scale_colour_solarized("blue") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(axis.text.y = element_text(angle = 0))
  
  p # возвращаем ggplot
}

plot_github_ts_data <- function(raw.df, ddepth = 1) {
  
  # фильтруем данные
  df <- raw.df %>%
    filter(work.status) %>%
    filter(timestamp < lubridate::now()) %>%
    filter(timestamp > floor_date(lubridate::now() - days(ddepth), unit = "day"))
  
  # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
  p <- ggplot(df, aes(x = timestamp, y = value, colour = name)) + 
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
  
  p # возвращаем ggplot
}

plot_ts_data_old <- function(raw.df, ddepth = 1) {

  df <- raw.df %>%
    filter(timegroup < lubridate::now()) %>%
    filter(timegroup > floor_date(lubridate::now() - days(ddepth), unit = "day"))
  
  avg.df <- df %>%
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

plot_weather_data <- function(raw.df, ddepth = 1) {
  
  df <- raw.df %>%
    filter(timegroup > floor_date(lubridate::now() - days(ddepth), unit = "day"))
  
  # разметим данные на прошлое и будущее. будем использовать для цветовой группировки
  df['time.pos'] <- ifelse(df$timestamp < now(), "PAST", "FUTURE")
  df$temp[df$time.pos == "FUTURE"] <- NA # в будущем нет актуальных
  
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  plot_palette <- brewer.pal(n = 8, name = "Paired") 
  
  # https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
  p1 <- ggplot(df, aes(timestamp, temp)) +
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
  p2 <- ggplot(df, aes(timestamp, precipitation)) +
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

plot_cweather <- function() {
  
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
      temp = round(r$main$temp - 273, 1), # пересчитываем из кельвинов в градусы цельсия
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
  
  p # возвращаем ggpmap!
}
  
draw_field_ggmap <- function(sensors.df) {
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
  
  # ================================ вычисляем тепловую карту
  
  # структура sensors должна быть предельно простая: lon, lat, val. Алгоритм расчитан именно на это
  smap.df <- sensors.df %>%
    ungroup() %>% # убрали группировку по name
    select(lon, lat, value) %>%
    rename(val = value)
  
  # print(smap.df)
  
  # данных крайне мало, чтобы не было сильных перепадов принудительно раскидаем 
  # по периметру прямоугольника сенсоры с минимальным значением влажности. (мы там не поливаем)
  # сделаем периметр по размеру прямоугольника отображения карты
  hdata <- data.frame(expand.grid(
    lon = seq(attr(fmap,"bb")$ll.lon, attr(fmap,"bb")$ur.lon, length = 10),
    lat = c(attr(fmap,"bb")$ll.lat, attr(fmap,"bb")$ur.lat),
    val = min(smap.df$val)
  ))
  
  vdata <- data.frame(expand.grid(
    lon = c(attr(fmap,"bb")$ll.lon, attr(fmap,"bb")$ur.lon),
    lat = seq(attr(fmap,"bb")$ll.lat, attr(fmap,"bb")$ur.lat, length = 10),
    val = min(smap.df$val)
  ))
  
 
  tdata <- rbind(smap.df, hdata, vdata)
  # print(tdata)
  
  # smap.df <- tdata
  # теперь готовим матрицу для градиентной заливки
  # берем идеи отсюда: http://stackoverflow.com/questions/24410292/how-to-improve-interp-with-akima
  # и отсюда: http://www.kevjohnson.org/making-maps-in-r-part-2/
  fld <- interp(tdata$lon, tdata$lat, tdata$val,
                xo = seq(min(tdata$lon), max(tdata$lon), length = 20),
                yo = seq(min(tdata$lat), max(tdata$lat), length = 20),
                duplicate = "mean", # дубликаты возникают по углам искуственного прямоугольника
                #linear = TRUE, #FALSE (после того, как добавили внешний прямоугольник, можно)
                linear = FALSE,
                extrap = TRUE)
  
  # превращаем в таблицу значений для комбинаций (x, y)
  # хранение колоночного типа, адресация (x, y) 
  # поэтому для делается хитрая развертка -- бегущий x раскладывается по фиксированным y, как оно хранится
  dInterp <- data.frame(expand.grid(x = fld$x, y = fld$y), z = c(fld$z)) 
  # при моделировании сплайнами, 
  # в случае крайне разреженных данных могут быть косяки со слишком кривыми аппроксимациями
  # dInterp$z[dInterp$z < min(smap.df$val)] <- min(smap.df$val)
  # dInterp$z[dInterp$z > max(smap.df$val)] <- max(smap.df$val)
  dInterp$z[is.nan(dInterp$z)] <- min(smap.df$val)
  
  # ========================================генерируем карту
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  plot_palette <- brewer.pal(n = 8, name = "Dark2")
  
  # а теперь попробуем отобразить растром, понимая все потенциальные проблемы
  # проблемы хорошо описаны здесь: https://groups.google.com/forum/embed/#!topic/ggplot2/nqzBX22MeAQ
  gm <- ggmap(fmap, extent = "device", legend = "topleft") +
    geom_tile(data = dInterp, aes(x, y, fill = z), alpha = 0.5, colour = NA) +
    #geom_raster(data = dInterp, aes(x, y, fill = z), alpha = 0.5) +
    #coord_cartesian() +
    scale_fill_distiller(palette = "Spectral") + #color -- цвет линий
    stat_contour(data = dInterp, aes(x, y, z = z), bins = 4, color="white", size=0.5) +
    # To use for line and point colors, add
    scale_colour_manual(values = plot_palette) +
    geom_point(data = sensors.df, size = 4, alpha = 0.8, aes(x = lon, y = lat, colour = level)) +
    geom_text(data = sensors.df, aes(lon, lat, label = round(value, digits = 1)), hjust = 0.5, vjust = -1) +
    theme_bw() +
    # убираем все отметки
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  
  gm # возвращаем ggpmap!
}



