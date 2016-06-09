
get_timeframe <- function(days_back = 7, days_forward = 3){
  # если по каким-либо причинам наверху не определились с прогнозом (NA), 
  # то полагаем что он есть и он равен базовому горизонту
  days_formard <- ifelse(is.na(days_forward), 3, days_forward)
  min_lim <- floor_date(now() - days(days_back), unit = "day")
  # поскольку будущее округляем вниз, то надо добавить еще сутки (+1)
  max_lim <- ceiling_date(now() + days(days_forward), unit = "day") 
  timeframe <- c(min_lim, max_lim)
  
  timeframe
}
  

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

hgroup.enum1 <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения. 
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  tick_time <- date
  n <- floor(hour(tick_time) / time.bin)
  floor_date(tick_time, unit = "day") + hours(n * time.bin)
}

hgroup.enum <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения. 
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  
  tick_time <- date
  if (time.bin < 1 & !(time.bin %in% c(0.25, 0.5))) time.bin = 1
  n <- floor((hour(tick_time)*60 + minute(tick_time))/ (time.bin * 60))
  floor_date(tick_time, unit = "day") + minutes(n * time.bin *60)
}



prepare_raw_weather_data <- function() {
  # получаем исторические данные по погоде из репозитория Гарика --------------------------------------------------------
  # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
  req <- try({
    curl_fetch_memory("https://raw.githubusercontent.com/iot-rus/Moscow-Lab/master/weather.txt")
    # status_code == 200
    # class(try-error)
  })
  
  # проверим только 1-ый элемент класса, поскльку при разных ответах получается разное кол-во элементов
  if(class(req)[[1]] == "try-error" || req$status_code != 200) {
    flog.info(paste0("Error in prepare_raw_weather:", class(req)))
    # создадим фейковый ответ, структуру ответа задаем ручками :((, 
    # ее нужно будет каждый раз корректировать при изменениях структуры данных
    # фиктивная структура по типу weather.df
    r <- structure(list(temp = numeric(), pressure = numeric(), himidity = numeric(),
                        timestamp = as.POSIXct(), rain3h = numeric(), human_time = as.POSIXct(),
                        timegroup = as.POSIXct(), time.pos = character()), class = "data.frame")
    return(r)
  }
  
  # ответ есть, и он корректен. В этом случае осуществляем пребразование 
  
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
  # поскольку историю мы сохраняем сами из данных текущих запросов, то
  # rain$3h -- Rain volume for the last 3 hours (http://openweathermap.org/current#parameter)
  whist.df$rain3h <- data$res$rain[['3h']]
  whist.df$human_time <- as.POSIXct(whist.df$timestamp, origin='1970-01-01')
  # browser()  
  
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
    # мм осадков за предыдущие 3 часа (Rain volume for last 3 hours, mm)
    # http://openweathermap.org/forecast5#parameter
    ldate$rain3h <- getElement(x, 'rain')[['3h']]
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

  weather.df
}

get_weather_df <- function(back_days = 7, forward_days = 3) {

  weather.df <- prepare_raw_weather_data()

  # browser()
  # причешем данные для графика у Паши + проведем усреднение по часовым группам
  # есть нюансы, связанные с выдачей данных из прогноза. 
  # rain3h соотв. прогнозу осадков в мм, на предыдущих три часа
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

calc_rain_per_date <- function(weather.df) {
  # считаем осадки за сутки ------------------------------
  # полагаем, что идентичность выпавших осадков с точностью до третьего знака просто означает дублирование показаний!!!!
  dfw0 <- data.frame(timestamp = weather.df$timestamp, rain3h = weather.df$rain3h) %>%
    filter(!is.na(rain3h)) %>% # записи без дождя нас вообще не интересуют
    distinct() %>% # полностью дублирующиеся записи также неинтересны
    # mutate(date = lubridate::date(timestamp)) %>%
    mutate(date = as.Date(timestamp)) %>%
    group_by(date, rain3h) %>% # собираем агрегаты по суткам, а потом по повторяющимся значениям, 
    # может быть погрешность по переходам через сутки, но при группировке по значениям можем случайно объединить данных с разных дат
    # в каждой группе посчитаем временную протяженность события
    arrange(timestamp) %>%
    mutate (dtime = as.numeric(difftime(timestamp, min(timestamp), unit = "min")))
  
  # теперь мы можем проверить, чтобы максимальное значение в группе не превышало 180 мин (3 часа)
  # поглядел на данные, таких групп нет за месяц не нашел, решил пока для простоты забить
  dfw1 <- dfw0 %>% 
    # в каждой группе выберем значение с минимальным временем измерения
    filter(timestamp == min(timestamp)) %>% # см. допущение об идентичности показаний
    ungroup() %>%
    arrange(timestamp)
  
  # а теперь посчитаем агрегаты по суткам
  dfw2 <- dfw1 %>%
    select(-dtime) %>%
    group_by(date) %>%
    summarise(rain = sum(rain3h)) %>% # пытаемся высчитать агрегат за сутки
    ungroup %>%
    #mutate(timegroup = as.numeric(as.POSIXct(date, origin='1970-01-01'))) %>%
    mutate(timestamp.human = force_tz(with_tz(as.POSIXct(date), tz = "GMT"), tz = "Europe/Moscow")) %>%
    mutate(timestamp = as.numeric(timestamp.human)) %>%
    arrange(date)

  dfw2
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
  # подгружаем данные по сенсорам. Это было в старом формате.
  #x <- read.csv( curl("https://github.com/iot-rus/Moscow-Lab/raw/master/result_moisture.txt") )
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
  
  
  # проверим только 1-ый элемент класса, поскльку при разных ответах получается разное кол-во элементов
  if(class(temp.df)[[1]] != "try-error") {
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

load_github_field2_data <- function() {
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
      # получим временную метку
      mutate(timestamp = ymd_hms(paste(date, time), truncated = 3, tz = "Europe/Moscow")) %>%
      # упростим имя сенсора
      mutate(label = gsub(".*:", "", rawname, perl = TRUE)) %>%
      # и разделим на имя и адрес
      separate(label, c('ipv6', 'name'), sep = '-', remove = TRUE) %>%
      select(timestamp, name, type, value, voltage, lat, lon, pin) %>%
      mutate(location = "Moscow Lab") 
    
    flog.info("Sensors data from GitHub recieved. Last records:")
    flog.info(capture.output(print(head(arrange(df, desc(timestamp)), n = 4))))

    # или даже так
    levs <- list(step = c(0, 1700, 2270, 2330, 2390, 2450, 3000) * 1000, 
                 category = c('WET++', 'WET+', 'WET', 'NORM', 'DRY', 'DRY+', 'DRY++'))
    
    df <- df %>%
      # откалибруем всплески
      mutate(work.status = (type == 'MOISTURE' & 
                              voltage >= levs$step[2] & 
                              voltage <= levs$step[length(levs$step)])) %>%
      # и вернем для влажности в value редуцированный вольтаж
      mutate(value  = ifelse(type == 'MOISTURE', voltage/1000, value))

    df$level <- NA
    for (i in 1:(length(levs$step) - 1)){
      # print(paste0("i = ", i, " ", levs$category[i]))
      df$level[df$type == 'MOISTURE' & 
                 df$voltage > levs$step[i] &
                 df$voltage <= levs$step[i+1]] <- levs$category[i]
    }
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

plot_github_ts3_data <- function(df, timeframe, tbin = 4) {
  # timeframe -- [POSIXct min, POSIXct max]
  
  # фильтруем данные. сгруппируем по временным интервалам
  # и удалим все данные с NA. Из-за неполных данных возникают всякие косяки
  # [filter for complete cases in data.frame using dplyr (case-wise deletion)](http://stackoverflow.com/questions/22353633/filter-for-complete-cases-in-data-frame-using-dplyr-case-wise-deletion)
  raw.df <- df %>%
    filter(complete.cases(.)) %>%
    mutate(timegroup = hgroup.enum(timestamp, time.bin = tbin)) %>%
    filter(timegroup >= timeframe[1]) %>%
    filter(timegroup <= timeframe[2])
  
  lims <- timeframe  
  # проведем усреднение по временным группам, если измерения проводились несколько раз в течение этого времени
  # усредняем только по рабочим датчикам
  
  avg.df <- raw.df %>%
    filter(work.status) %>%
    group_by(location, name, timegroup) %>%
    summarise(value.mean = mean(value), value.sd = sd(value)) %>%
    ungroup() # очистили группировки
  
  # готовим графическое представление ----------------------------------------
  plot_palette <- brewer.pal(n = 5, name = "Blues")
  plot_palette <- wes_palette(name = "Moonrise2") # https://github.com/karthik/wesanderson
  
  # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
  p <- ggplot(avg.df, aes(x = timegroup, y = value.mean)) +
    # http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
    scale_fill_brewer(palette="Dark2", direction = -1, guide = FALSE) +
    scale_color_brewer(palette="Dark2", direction = -1, name = "Сенсор", guide = guide_legend(reverse = FALSE, fill = FALSE)) + 
  
    # scale_fill_manual(values = plot_palette, guide = FALSE) + # легенду по заполнению отключаем
    # scale_color_manual(values = plot_palette, name = "Сенсор", guide = guide_legend(reverse = FALSE, fill = FALSE)) +
  
    #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    # рисуем разрешенный диапазон
    geom_ribbon(aes(x = timegroup, ymin = 70, ymax = 90), linetype = 'blank', 
                fill = "olivedrab3", alpha = 0.4) +
    geom_ribbon(
      aes(ymin = value.mean - value.sd, ymax = value.mean + value.sd, fill = name),
      alpha = 0.3
      ) +
    geom_line(aes(colour = name), lwd = 1.2) +
    # точки сырых данных
    geom_point(data = raw.df, aes(x = timestamp, y = value, colour = name), shape = 1, size = 2) +
    geom_point(aes(colour = name), shape = 19, size = 3) + # усредненные точки
    geom_hline(yintercept = c(70, 90), lwd = 1, linetype = 'dashed') +
    # scale_x_datetime(labels = date_format(format = "%d.%m%n%H:%M", tz = "Europe/Moscow"),
    #                  breaks = date_breaks('4 hour')) +
    # текщуее время отобразим
    geom_vline(xintercept = as.numeric(now()), linetype = "dotted", color = "yellowgreen", lwd = 1.1) +
    scale_x_datetime(labels = date_format("%d.%m", tz = "Europe/Moscow"),
                     breaks = date_breaks("1 days"), 
                     #minor_breaks = date_breaks("6 hours"),
                     limits = lims) +
    
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
    # theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
    # theme(axis.text.y = element_text(angle = 0)) +
    # убрали заливку, см. stackoverflow.com/questions/21066077/remove-fill-around-legend-key-in-ggplot
    guides(color = guide_legend(override.aes = list(fill = NA)))

  p # возвращаем ggplot
}

plot_github_ts4_data <- function(df, timeframe, tbin = 4, expand_y = FALSE) {
  # рисуем новый вид графика после проведения калибровочных экспериментов
  # timeframe -- [POSIXct min, POSIXct max]
  
  # Полагаем, что у нас всегда ненулевой горизонт прогноза
  # т.о., если выбрано отсутствие синхронизации, то правая граница диапазона находится в конце текущего дня
  force_rtime <- ifelse(difftime(timeframe[2], now(), unit = "min") > 24 * 60, FALSE, TRUE) 

  # Удалим все данные с NA. Из-за неполных данных возникают всякие косяки
  # [filter for complete cases in data.frame using dplyr (case-wise deletion)](http://stackoverflow.com/questions/22353633/filter-for-complete-cases-in-data-frame-using-dplyr-case-wise-deletion)
  # И сгруппируем по временным интервалам
  raw.df <- df %>%
    filter(complete.cases(.)) %>%
    mutate(timegroup = hgroup.enum(timestamp, time.bin = tbin))
    
  # если нет синхронизации, то подгоним правый край до максимальной даты измерения
  if (force_rtime) timeframe[2] <- max(raw.df$timegroup)
  
  flog.info(paste0("plot_github_ts4: force = ", force_rtime, 
                   " timeframe: [", timeframe[1], ", ", timeframe[2], "]"))
  
  # фильтруем данные
  raw.df <- raw.df %>%
    filter(timegroup >= timeframe[1]) %>%
    filter(timegroup <= timeframe[2])
  
  lims <- timeframe  
  # проведем усреднение по временным группам, если измерения проводились несколько раз в течение этого времени
  # усредняем только по рабочим датчикам
  
  avg.df <- raw.df %>%
    filter(work.status) %>%
    group_by(location, name, timegroup) %>%
    summarise(value.mean = mean(value), value.sd = sd(value)) %>%
    ungroup() # очистили группировки
  
  # готовим графическое представление ----------------------------------------
  plot_palette <- brewer.pal(n = 5, name = "Blues")
  plot_palette <- wes_palette(name = "Moonrise2") # https://github.com/karthik/wesanderson
  
  levs <- list(step = c(1700, 2210, 2270, 2330, 2390, 2450, 2510), 
               category = c('WET++', 'WET+', 'WET', 'NORM', 'DRY', 'DRY+', ''))
  if(!expand_y){
    # рисуем график только по DRY+ -- WET+ значениям
    levs <- list(step = levs$step[-1], category = levs$category[-1])
    
  }
  df.label <- (data.frame(x = timeframe[1], y = levs$step+30, text = levs$category))

  # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
  p <- ggplot(avg.df, aes(x = timegroup, y = value.mean)) +
    # http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
    scale_fill_brewer(palette="Dark2", direction = -1, guide = FALSE) +
    scale_color_brewer(palette="Dark2", direction = -1, name = "Сенсор", guide = guide_legend(reverse = FALSE, fill = FALSE)) + 
    
    # scale_fill_manual(values = plot_palette, guide = FALSE) + # легенду по заполнению отключаем
    # scale_color_manual(values = plot_palette, name = "Сенсор", guide = guide_legend(reverse = FALSE, fill = FALSE)) +
    
    #scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    # рисуем разрешенный диапазон
    # geom_ribbon(aes(x = timegroup, ymin = 70, ymax = 90), linetype = 'blank', 
    #             fill = "olivedrab3", alpha = 0.4) +
    geom_ribbon(aes(x = timegroup, ymin = levs$step[4], ymax = levs$step[3]), linetype = 'blank', 
                fill = "olivedrab3", alpha = 0.4) +
    geom_ribbon(
      aes(ymin = value.mean - value.sd, ymax = value.mean + value.sd, fill = name),
      alpha = 0.3
    ) +
    geom_line(aes(colour = name), lwd = 1.2) +
    # точки сырых данных
    geom_point(data = raw.df, aes(x = timestamp, y = value, colour = name), shape = 1, size = 2) +
    geom_point(aes(colour = name), shape = 19, size = 3) + # усредненные точки
    geom_hline(yintercept = levs$step, lwd = 1, linetype = 'dashed') +
    # scale_x_datetime(labels = date_format(format = "%d.%m%n%H:%M", tz = "Europe/Moscow"),
    #                  breaks = date_breaks('4 hour')) +
    # текщуее время отобразим
    geom_vline(xintercept = as.numeric(now()), linetype = "dotted", color = "yellowgreen", lwd = 1.1)
    
    
  # в зависимости от диапазона отображения меняем параметры надписей оси x
  if (force_rtime) {
    p <- p +
      scale_x_datetime(labels = date_format("%H:%M", tz = "Europe/Moscow"),
                       breaks = date_breaks("1 hour"), 
                       limits = lims)
  } else {
    p <- p +
      scale_x_datetime(labels = date_format("%d.%m", tz = "Europe/Moscow"),
                       breaks = date_breaks("1 days"), 
                       minor_breaks = date_breaks("6 hours"),
                       limits = lims)
  }
    
    # minor_breaks = date_breaks('1 hour')
    # добавляем нерабочие сенсоры
    # geom_point(data = raw.df %>% filter(!work.status), aes(x = timegroup, y = value),
    #            size = 3, shape = 21, stroke = 0, colour = 'red', fill = 'yellow') +
    # geom_point(data = raw.df %>% filter(!work.status), aes(x = timegroup, y = value),
    #            size = 3, shape = 13, stroke = 1.1, colour = 'red') +
  p <- p +    
    theme_igray() +
    geom_label(data = df.label, aes(x = x, y = y, label = text)) +
    # scale_colour_tableau("colorblind10", name = "Влажность\nпочвы") +
    # scale_color_brewer(palette = "Set2", name = "Влажность\nпочвы") +
    # ylim(0, 100) +
    # scale_y_reverse(limits = c(head(levs$step, 1), tail(levs$step, 1))) +
    scale_y_reverse(limits = c(tail(levs$step, 1), head(levs$step, 1))) +
    xlab("Время и дата измерения") +
    ylab("Влажность почвы") +
    # theme_solarized() +
    # scale_colour_solarized("blue") +
    # theme(legend.position=c(0.5, .2)) +
    theme(legend.position = "top") +
    # theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
    # theme(axis.text.y = element_text(angle = 0)) +
    # убрали заливку, см. stackoverflow.com/questions/21066077/remove-fill-around-legend-key-in-ggplot
    guides(color = guide_legend(override.aes = list(fill = NA)))
  
  p # возвращаем ggplot
}

plot_real_weather2_data <- function(weather.df, rain.df, timeframe) {
  # timeframe -- [POSIXct min, POSIXct max]
  # агрегат осадков за сутки
  # чтобы график нарисовался столбиками строго по дням, необходимо пропущенные дни добить нулями
  dft <- data.frame(date = seq.Date(as.Date(timeframe[1]), as.Date(timeframe[2]), by = "1 day"),
                    rain2 = 0)
  df2 <- dft %>%
    left_join(rain.df, by = "date") %>%
    mutate(rain = rain2 + ifelse(is.na(rain), 0, rain)) %>%
    select(date, rain) %>%
    mutate(timestamp = force_tz(with_tz(as.POSIXct(date), tz = "GMT"), tz = "Europe/Moscow")) %>%
    filter(timestamp >= timeframe[1]) %>%
    filter(timestamp <= timeframe[2])
  
  # погода
  df <- weather.df %>%
    filter(timegroup >= timeframe[1]) %>%
    filter(timegroup <= timeframe[2])

  lims <- timeframe
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
    scale_x_datetime(labels = date_format("%d.%m", tz = "Europe/Moscow"), 
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
  # по просьбе Игоря даем сдвижку к столбику + 12 часов для попадания столбика ровно в сутки
  p3 <- pp + 
    geom_bar(data = df2 %>% mutate(timestamp = timestamp + hours(12)), 
             aes(timestamp, rain), fill = brewer.pal(n = 9, name = "Blues")[4], alpha = 0.5, stat="identity") +
    ylim(0, NA) +
    ylab("Осадки\n(дождь), мм")
  
  # grid.arrange(p1, p2, p3, ncol = 1) # возвращаем ggplot
  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "first"))
  
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
      temp = round(r$main$temp - 273.15, 0), # пересчитываем из кельвинов в градусы цельсия
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

