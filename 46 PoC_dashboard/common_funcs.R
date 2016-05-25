
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

      flog.info((paste0("Element:", el)))
      dt <-
        abs(as.numeric(difftime(el, round_date(el, unit = "day"), unit = "min")))
      # str(dt)
      if (is.na(dt)) {
        ret <- NA
      }
      else {
        if (dt < 130) {
          # допустим разброс в 130 минут
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

hgroup.enum0 <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток +-1/2 интервала, к точке измерения. 
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  tick_time <- date + minutes(time.bin * 60)/2 # сдвигаем на пол интервала вперед
  n <- floor(hour(tick_time) / time.bin)
  floor_date(tick_time, unit = "day") + hours(n * time.bin)
}

hgroup.enum <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения. 
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  tick_time <- date
  n <- floor(hour(tick_time) / time.bin)
  floor_date(tick_time, unit = "day") + hours(n * time.bin)
}

get_weather_df <- function(back_days = 7, forward_days = 3) {
  # получаем исторические данные по погоде из репозитория Гарика --------------------------------------------------------
  # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
  req <- curl_fetch_memory("https://raw.githubusercontent.com/iot-rus/Moscow-Lab/master/weather.txt")
  wrecs <- rawToChar(req$content) # weather history
  # wh_json <- gsub('\\\"', "'", txt, perl = TRUE) 
  # заменим концы строк на , и добавим шапочку и окончание для формирования семантически правильного json
  # последнюю ',' надо удалить, может такое встретиться (перевод строки)
  tmp <- paste0('{"res":[', gsub("\\n", ",\n", wrecs, perl = TRUE), ']}')
  wh_json <- gsub("},\n]}", "}]}", tmp)
  # t <- cat(wh_json)
  # write(wh_json, file="./export/wh_json.txt")
  data <- fromJSON(wh_json)
  
  whist.df <- data$res$main
  whist.df$timestamp <- data$res$dt
  
  # t0 <- '{"coord":{"lon":37.61,"lat":55.76},"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"base":"cmc stations","main":{"temp":291.77,"pressure":1012,"humidity":72,"temp_min":290.15,"temp_max":295.35},"wind":{"speed":4,"deg":340},"clouds":{"all":0},"dt":1464008912,"sys":{"type":1,"id":7323,"message":0.0031,"country":"RU","sunrise":1463965411,"sunset":1464025820},"id":524894,"name":"Moskva","cod":200}'
  # t1 <- '{"coord":{"lon":37.61,"lat":55.76},"weather":[{"id":800,"main":"Clear","description":"clear sky","icon":"01d"}],"base":"stations","main":{"temp":291.01,"pressure":1012,"humidity":72,"temp_min":289.15,"temp_max":292.15},"visibility":10000,"wind":{"speed":4,"deg":330},"clouds":{"all":0},"dt":1464007798,"sys":{"type":1,"id":7323,"message":0.0354,"country":"RU","sunrise":1463965412,"sunset":1464025819},"id":524894,"name":"Moskva","cod":200}'
  # t <- paste0('{"results":[', t0, ',', t1, ']}')
  # mdata <- fromJSON(t)
  
  # head(wh_json)
  
  # получаем прогноз через API --------------------------------------------------------
  url <- "api.openweathermap.org/data/2.5/"   
  MoscowID <- '524901'
  APPID <- '19deaa2837b6ae0e41e4a140329a1809'
  # reqstring <- paste0(url, "weather?id=", MoscowID, "&APPID=", APPID)
  reqstring <- paste0(url, "forecast?id=", MoscowID, "&APPID=", APPID) 
  resp <- GET(reqstring)
  if(status_code(resp) == 200){
    r <- content(resp)
  }
  
  # получаем погодные данные
  m <- r$list
  ll <- lapply(m, function(x){ 
    ldate <- getElement(x, 'main')
    ldate$timestamp <- getElement(x, 'dt')
    ldate$rain3h <- getElement(x, 'rain')[['3h']] ## мм осадков на следующие 3 часа
    ldate
  })
  l2 <- melt(ll)
  # нормализуем под колонки, которые есть в исторических данных
  l3 <- tidyr::spread(l2, L2, value) %>% 
    select(-L1, -temp_kf) %>%
    mutate(timestamp = as.integer(timestamp))
  
  # объединяем и вычищаем --------------------------------------------------------
  
  weather.df <- bind_rows(whist.df, l3) %>%
    select(-temp_max, -temp_min, -sea_level, -grnd_level) %>%
    distinct() %>% # удаляем дубли, которые навыдавал API
    mutate(temp = round(temp - 273.15, 1)) %>% # пересчитываем из кельвинов в градусы цельсия
    mutate(pressure = round(pressure * 0.75006375541921, 0)) %>% # пересчитываем из гектопаскалей (hPa) в мм рт. столба
    mutate(humidity = round(humidity, 0)) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin='1970-01-01')) %>%
    mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) # сделаем почасовую группировку
  
  # разметим данные на прошлое и будущее. будем использовать для цветовой группировки
  weather.df['time.pos'] <- ifelse(weather.df$timestamp < now(), "PAST", "FUTURE")
  
  # browser()
  # причешем данные для графика у Паши + проведем усреднение по часовым группам
  # есть нюансы, связанные с выдачей данных из прогноза. 
  # rain3h соотв. прогнозу осадков в мм, на след. три часа
  # за консистентность информации (нарезка тиков 3-х часовыми интервалами) отвечает API.
  # поэтому что mean, что sum -- все одно. timegroup для каждого прогнозного измерения должна быть ровно одна
  outw.df <- weather.df %>%
    filter(timegroup >= floor_date(now() - days(back_days), unit = "day")) %>%
    filter(timegroup <= ceiling_date(now() + days(forward_days), unit = "day")) %>%
    group_by(timegroup, time.pos) %>%
    summarise(temp = mean(temp), pressure = mean(pressure), humidity = mean(humidity), rain3h_av = mean(rain3h)) %>%
    ungroup
  
  # чтобы график не был разорванным, надо продублировать максимальную точку из PAST в группу FUTURE
  POI.df <- outw.df %>%
    filter(time.pos == 'PAST') %>%
    filter(timegroup == max(timegroup)) %>%
    mutate(time.pos = 'FUTURE')
  
  outw.df <- outw.df %>%
    bind_rows(POI.df) %>%
    arrange(timegroup)
  
  outw.df
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
      mutate(value = round(100 / (calibration_100 - calibration_0) * (voltage - calibration_0), 0)) %>%
      # откалибруем всплески
      mutate(work.status = (value >= 0 & value <= 100)) %>%
      # получим временную метку
      mutate(timestamp = ymd_hm(paste(date, time), tz = "Europe/Moscow")) %>%
      # упростим имя сенсора
      mutate(name = gsub(".*:", "", name, perl = TRUE)) %>%
      mutate(location = "Moscow Lab") %>%
      select(-calibration_0, -calibration_100, -voltage, -date, -time)
    
    flog.info("Sensors data from GitHub recieved. Last records:")
    flog.info(capture.output(print(head(arrange(df, desc(timestamp)), n = 4))))
    
  } else {
    df <- NA # в противном случае мы сигнализируем о невозможности обновить данные
    flog.error("GitHub connection error")
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
    scale_x_datetime(labels = date_format(format = "%d.%m\n%H:%M", tz = "Europe/Moscow"),
                     breaks = man.breaks, limits = man.lims) +
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

plot_github_ts2_data <- function(df, ddepth = 1, tbin = 4) {

  # фильтруем данные
  raw.df <- df %>%
    # filter(work.status) %>%
    filter(timestamp < lubridate::now()) %>%
    filter(timestamp > floor_date(lubridate::now() - days(ddepth), unit = "day")) %>%
    # сгруппируем по временным интервалам
    mutate(timegroup = hgroup.enum(timestamp, time.bin = tbin))

  # проведем усреднение по временным группам, если измерения проводились несколько раз в течение этого времени
  # усредняем только по рабочим датчикам
  
  avg.df <- raw.df %>%
    filter(work.status) %>%
    group_by(location, name, timegroup) %>%
    summarise(value.mean = mean(value), value.sd = sd(value)) %>%
    ungroup() # очистили группировки

  # выводим в json-файл  ----------------------------------------
  # http://stackoverflow.com/questions/25550711/convert-data-frame-to-json
  # avg_json <- jsonlite::toJSON(avg.df, pretty = TRUE)
  # запуск идет из папки с app.R
  # write(avg_json, file = "avg_df.json")

  # готовим графическое представление ----------------------------------------
  plot_palette <- brewer.pal(n = 5, name = "Blues")
  plot_palette <- wes_palette(name = "Moonrise2") # https://github.com/karthik/wesanderson

  # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
  p <- ggplot(avg.df, aes(x = timegroup, y = value.mean, colour = name)) +
    # http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
    # scale_fill_brewer(palette="Dark2") +
    # scale_color_brewer(palette="Dark2") +
    scale_color_manual(values = plot_palette) +
    scale_fill_manual(values = plot_palette) +
    #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    # рисуем разрешенный диапазон
    # geom_ribbon(aes(ymin = 70, ymax = 90), fill = "darkseagreen1") +
    geom_ribbon(aes(ymin = 70, ymax = 90), fill = "mediumaquamarine", alpha = 0.3) +
    geom_ribbon(aes(ymin = value.mean - value.sd, ymax = value.mean + value.sd, fill = name), 
                alpha = 0.3) +
    geom_line(lwd = 1.5) +
    geom_point(data = raw.df, aes(x = timestamp, y = value), shape = 1, size = 2) +
    geom_hline(yintercept = c(70, 90), lwd = 1.2, linetype = 'dashed') +
    geom_point(shape = 19, size = 3) +
    # scale_x_datetime(labels = date_format(format = "%d.%m%n%H:%M", tz = "Europe/Moscow"),
    #                  breaks = date_breaks('4 hour')) +
    scale_x_datetime(labels = date_format("%d.%m"), breaks = date_breaks("1 days"), minor_breaks = date_breaks("6 hours")) +
    
      # minor_breaks = date_breaks('1 hour')
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
    # theme(legend.position=c(0.5, .2)) +
    theme(legend.position = "top") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
    theme(axis.text.y = element_text(angle = 0))

  p # возвращаем ggplot
}

plot_github_ts_data <- function(df, ddepth = 1, tbin = 4) {
  
  # фильтруем данные
  raw.df <- df %>%
    filter(work.status) %>%
    filter(timestamp < lubridate::now()) %>%
    filter(timestamp > floor_date(lubridate::now() - days(ddepth), unit = "day"))
  
  # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
  p <- ggplot(raw.df, aes(x = timestamp, y = value, colour = name)) + 
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

plot_real_weather_data <- function(raw.df, ddepth = 1) {
  
  # надо дополнительно отфильтровать по глубине данных
  df <- raw.df %>%
    filter(timegroup >= floor_date(now() - days(ddepth), unit = "day")) # %>%
    #filter(timegroup <= ceiling_date(now() + days(forward_days), unit = "day")) %>%
  
  p1 <- ggplot(df, aes(timegroup, temp, colour = time.pos)) +
    # ggtitle("График температуры") +
    # scale_fill_brewer(palette="Set1") +
    # scale_fill_brewer(palette = "Paired") +
    scale_color_brewer(palette = "Paired") +
    # geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
    # geom_point(shape = 1, size = 3) +
    # geom_line(lwd = 1, linetype = 'dashed', color = "red") +
    scale_x_datetime(labels = date_format("%d.%m"), breaks = date_breaks("1 days"), minor_breaks = date_breaks("6 hours")) +
    geom_line(lwd = 1.2) +
    theme_igray() +
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
    scale_x_datetime(labels = date_format("%d.%m"), breaks = date_breaks("1 days"), minor_breaks = date_breaks("6 hours")) +
    geom_line(lwd = 1.2) +
    theme_igray() +
    theme(legend.position="none") +
    ylim(0, 100) +
    xlab("Дата") +
    ylab("Влажность воздуха, %")
  
  grid.arrange(p1, p2, ncol = 1) # возвращаем ggplot
}

prepare_sesnors_mapdf <- function(input.df, slicetime) {
  df <- input.df %>%
    filter(timestamp <= slicetime) %>%
    group_by(name) %>%
    filter(timestamp == max(timestamp)) %>%
    mutate(delta = round(as.numeric(difftime(slicetime, timestamp, unit = "min")), 0)) %>%
    arrange(name) %>%
    ungroup() %>%
    # рабочий статус также определяется тем, насколько давно мы видели показания от конкретного сенсора
    mutate(work.status = (delta < 60))
  
  
  # откатегоризируем
  df <- within(df, {
    level <- NA
    level[value >= 0 & value <= 33] <- "Low"
    level[value > 33  & value <= 66] <- "Normal"
    level[value > 66  & value <= 100] <- "High"
  })
  
  # при группировке по Lev по умолчанию, порядок следования строк осуществляется по алфавиту
  # ggplot(sensors.df, aes(x = lat, y = lon, colour = level)) + 
  #  geom_point(size = 4)
  
  # пытаемся изменить группировку
  # http://docs.ggplot2.org/current/aes_group_order.html
  # сделаем из текстовых строк factor и их принудительно отсортируем
  # http://www.ats.ucla.edu/stat/r/modules/factor_variables.htm
  
  # у нас два критерия разделения -- диапазон значений и работоспособность.
  # диапазон измерений -- цвет, работоспособность -- форма
  
  sensors.df <- df %>%
    rename(level.unordered = level) %>%
    # mutate(lev.of = ordered(lev, levels = c('Low', 'Normal', 'High'))) %>%
    mutate(level = factor(level.unordered, levels = c('High', 'Normal', 'Low'))) %>%
    mutate(work.status = work.status & !is.na(level)) # что не попало в категорию также считается нерабочим
  
  # возвращаем преобразованный df
  sensors.df
}

draw_field_ggmap <- function(sensors.df, heatmap = TRUE) {
  
  fmap <-
    get_map(
      # enc2utf8("Москва, Зоологическая 2"),
      # "Москва, Зоологическая 2", # надо понимать из какой кодировки грузим
      
      c(median(sensors.df$lon), median(sensors.df$lat)), # будем запрашивать по координатам c(lon, lat)
      language = "ru-RU",
      source = "stamen", maptype = "watercolor", 
      # source = "stamen", maptype = "toner-hybrid",
      # source = "stamen", maptype = "toner-lite",
      # source = "google", maptype = "terrain",
      # source = "osm", maptype = "terrain-background",
      # source = "google", maptype = "hybrid",
      # source = "stamen", maptype = "toner-2011",
      zoom = 16
    )
  
  # определяем, рисовать ли тепловую карту
  if (heatmap) {
    # ================================ вычисляем тепловую карту
    
    # структура sensors должна быть предельно простая: lon, lat, val. Алгоритм расчитан именно на это
    smap.df <- sensors.df %>%
      select(lon, lat, value) %>%
      rename(val = value)
    
    # print(smap.df)
    
    # данных крайне мало, чтобы не было сильных перепадов принудительно раскидаем
    # по периметру прямоугольника сенсоры с минимальным значением влажности. (мы там не поливаем)
    # сделаем периметр по размеру прямоугольника отображения карты
    hdata <- data.frame(expand.grid(
      lon = seq(attr(fmap, "bb")$ll.lon, attr(fmap, "bb")$ur.lon, length = 10),
      lat = c(attr(fmap, "bb")$ll.lat, attr(fmap, "bb")$ur.lat),
      val = min(smap.df$val)
    ))
    
    vdata <- data.frame(expand.grid(
      lon = c(attr(fmap, "bb")$ll.lon, attr(fmap, "bb")$ur.lon),
      lat = seq(attr(fmap, "bb")$ll.lat, attr(fmap, "bb")$ur.lat, length = 10),
      val = min(smap.df$val)
    ))
    
    
    tdata <- rbind(smap.df, hdata, vdata)
    # print(tdata)
    
    # smap.df <- tdata
    # теперь готовим матрицу для градиентной заливки
    # берем идеи отсюда: http://stackoverflow.com/questions/24410292/how-to-improve-interp-with-akima
    # и отсюда: http://www.kevjohnson.org/making-maps-in-r-part-2/
    fld <- interp(
      tdata$lon,
      tdata$lat,
      tdata$val,
      xo = seq(min(tdata$lon), max(tdata$lon), length = 100),
      yo = seq(min(tdata$lat), max(tdata$lat), length = 100),
      duplicate = "mean",
      # дубликаты возникают по углам искуственного прямоугольника
      #linear = TRUE, #FALSE (после того, как добавили внешний прямоугольник, можно)
      linear = FALSE,
      extrap = TRUE
    )
    
    # превращаем в таблицу значений для комбинаций (x, y)
    # хранение колоночного типа, адресация (x, y)
    # поэтому для делается хитрая развертка -- бегущий x раскладывается по фиксированным y, как оно хранится
    dInterp <-
      data.frame(expand.grid(x = fld$x, y = fld$y), z = c(fld$z))
    # при моделировании сплайнами,
    # в случае крайне разреженных данных могут быть косяки со слишком кривыми аппроксимациями
    # dInterp$z[dInterp$z < min(smap.df$val)] <- min(smap.df$val)
    # dInterp$z[dInterp$z > max(smap.df$val)] <- max(smap.df$val)
    dInterp$z[is.nan(dInterp$z)] <- min(smap.df$val)
  }
  
  # ======================================== генерируем карту
  # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
  plot_palette <- brewer.pal(n = 8, name = "Dark2")
  cfpalette <- colorRampPalette(c("white", "blue"))
  
  # а теперь попробуем отобразить растром, понимая все потенциальные проблемы
  # проблемы хорошо описаны здесь: https://groups.google.com/forum/embed/#!topic/ggplot2/nqzBX22MeAQ
  gm <- ggmap(fmap, extent = "normal", legend = "topleft", darken = c(0.3, "white")) # осветлили карту
  # legend = device
  if (heatmap){
    gm <- gm +
      # geom_tile(data = dInterp, aes(x, y, fill = z), alpha = 0.5, colour = NA) +
      geom_raster(data = dInterp, aes(x, y, fill = z), alpha = 0.5) +
      coord_cartesian() +
      # scale_fill_distiller(palette = "Spectral") + # http://docs.ggplot2.org/current/scale_brewer.html
      # scale_fill_distiller(palette = "YlOrRd", breaks = pretty_breaks(n = 10))+ #, labels = percent) +
      # scale_fill_gradientn(colours = brewer.pal(9,"YlOrRd"), guide="colorbar") +
      scale_fill_gradientn(colours = c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#0571B0", "#1A9641", "#D7191C"), 
                           limits = c(0, 100), breaks = c(25, 40, 55, 70, 85), guide="colorbar") +
      # scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) + # минимум -- белый    stat_contour(data = dInterp, aes(x, y, z = z), bins = 4, color="white", size=0.5) +
      # To use for line and point colors, add
      stat_contour(data = dInterp, aes(x, y, z = z), bins = 4, lwd = 1, color="blue")
  }
  
  work.df <- sensors.df %>% filter(work.status)
  broken.df <- sensors.df %>% filter(!work.status)
  
  # рисуем показания по рабочим сенсорам
  if (nrow(work.df) > 0){
    gm <- gm +
      # scale_colour_manual(values = plot_palette) +
      scale_color_manual(values = c("royalblue", "palegreen3", "sienna1"),
                         name = "Влажность\nпочвы") +
      geom_point(data = work.df, size = 4, alpha = 0.8,
        aes(x = lon, y = lat, colour = level))
  }
  # отдельно отрисовываем нерабочие сенсоры
  if (nrow(broken.df) > 0){
    gm <- gm +
      geom_point(data = broken.df, size = 4, shape = 21, 
                 stroke = 1, colour = 'black', fill = 'gold') +
      geom_point(data = broken.df, size = 4, shape = 13, 
                 stroke = 1, colour = 'black') +
      geom_text(data = broken.df, aes(lon, lat, label = paste0(delta, " мин"), 
                                      hjust = 0.5, vjust = 1.8), size = rel(3))
  }
  # тематическое оформление
  gm <- gm +
    geom_text(data = sensors.df, aes(lon, lat, label = round(value, digits = 1)),
              hjust = 0.5, vjust = -1) +
    theme_bw() +
    # убираем все отметки
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          # legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  
  gm # возвращаем ggpmap!
}


plot_cweather <- function() {
  
  url <- "api.openweathermap.org/data/2.5/"   
  MoscowID <- '524901'
  APPID <- '19deaa2837b6ae0e41e4a140329a1809'
  reqstring <- paste0(url, "weather?id=", MoscowID, "&APPID=", APPID) 
  resp <- GET(reqstring)
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
    flog.info(paste0("Погода запрошена успешно"))
    flog.info(capture.output(print(d)))
    flog.info('--------------------------------')
  }
  
  df <- data.frame(x = c(0, 1), y = c(0, 1))
  
  # windowsFonts(verdana = "TT Verdana")
  # windowsFonts(geinspira = "GE Inspira")
  # windowsFonts(corbel = "Corbel")
  p <- ggplot(df, aes(x, y)) + 
    geom_point() +
    geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1), fill = "peachpuff") +
    geom_text(aes(.5, .8), label = paste0(d$temp, " C"), size = 20, color="blue") + #, family = "verdana") +
    geom_text(aes(.5, .5), label = paste0(d$pressure, " мм"), size = 8, color="blue") + #, family = "verdana") +
    geom_text(aes(.5, .3), label = paste0(d$humidity, " %"), size = 8, color="blue") + #, family = "verdana") +
    geom_text(aes(.5, .1), label = paste0(d$timestamp), size = 6, color="blue") + #, family = "verdana") +
    theme_dendro() # совершенно пустая тема
  
  p # возвращаем ggpmap!
}

# autoresize ==============================================
# взят отсюда: https://ryouready.wordpress.com/2012/08/01/creating-a-text-grob-that-automatically-adjusts-to-viewport-size/
resizingTextGrob <- function(..., max.font.size = 40) {
  gr <- grob(tg = textGrob(...), cl = "resizingTextGrob")
  # добавим свой доп. атрибут -- максимальный размер текста до которого масштабируем
  # str(gr)
  gr[['max.font.size']] <- max.font.size
  gr
}
drawDetails.resizingTextGrob <- function(x, recording = TRUE) { grid.draw(x$tg) }
preDrawDetails.resizingTextGrob <- function(x)
{
  h <- convertHeight(unit(1, "snpc"), "mm", valueOnly = TRUE)
  fs <- rescale(h, to = c(x$max.font.size, 7), from = c(50, 5))
  flog.info(paste0("h = ", h, ", fs = ", fs))
  browser()
  # pushViewport(viewport(gp = gpar(fontsize = fs, fontface = 'bold')))
  pushViewport(viewport(gp = gpar(fontsize = fs)))
}
postDrawDetails.resizingTextGrob <- function(x) { popViewport() }

# ============================================================================

plot_cweather_scaled <- function() {
  
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
  
  l1 <- resizingTextGrob(label = paste0(d$temp, " C"), max.font.size = 120)
  l2 <- resizingTextGrob(label = paste0(d$pressure, " мм"), max.font.size = 80)
  l3 <- resizingTextGrob(label = paste0(d$humidity, " %"), max.font.size = 80)
  l4 <- resizingTextGrob(label = paste0(d$timestamp), max.font.size = 60)
  
  p <- ggplot(df, aes(x, y)) + 
    geom_point() +
    geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1), fill = "peachpuff") +
    # geom_text(aes(.5, .8), label = paste0(d$temp, " C"), size = rel(40), color="blue", family = "verdana") +
    annotation_custom(grob = l1, xmin = 0.3, xmax = 0.7, ymin = .7, ymax = .9) +
    annotation_custom(grob = l2, xmin = 0.3, xmax = 0.7, ymin = .5, ymax = .7) +
    annotation_custom(grob = l3, xmin = 0.3, xmax = 0.7, ymin = .3, ymax = .5) +
    annotation_custom(grob = l4, xmin = 0.3, xmax = 0.7, ymin = .1, ymax = .3) +
    #geom_text(aes(.5, .5), label = paste0(d$pressure, " мм"), size = 16, color="blue", family = "verdana") +
    #geom_text(aes(.5, .3), label = paste0(d$humidity, " %"), size = 16, color="blue", family = "verdana") +
    #geom_text(aes(.5, .1), label = paste0(d$timestamp), size = rel(8), color="blue", family = "verdana") +
    theme_dendro() # совершенно пустая тема
  
  p # возвращаем ggpmap!
}

