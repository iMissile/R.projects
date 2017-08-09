hgroup.enum <- function(date, hour_bin=NULL, min_bin=5){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется hour.bin
  # отсчет измерений идет с 0:00
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  # если hour.bin=NULL, то идет привязка к интервалам min.bin, заданном в минутах
  # необходим пакет lubridate
  
  tick_time <- date
  
  if (is.null(hour_bin)){
    # привязываем к минутным интервалам
    n <- floor(minute(tick_time)/min_bin)
    dt <- floor_date(tick_time, unit="hour") + minutes(n * min_bin)
    
  }else{
    # привязываем к часовым интервалам
    if (hour_bin < 1 & !(hour_bin %in% c(0.25, 0.5))) hour_bin=1
    n <- floor((hour(tick_time)*60 + minute(tick_time))/ (hour_bin*60))
    dt <- floor_date(tick_time, unit="day") + minutes(n * hour_bin*60)
  }
  
  dt
}


buildReqFilter <- function(db_field, conditions, add=TRUE){
  ifelse(is.null(conditions) || conditions=="all", " ",
         stri_join(ifelse(add, " AND ", " "), db_field, " IN (",
                   stri_join(conditions %>% map_chr(~stri_join("'", .x, "'", sep="")),
                             sep = " ", collapse=","),
                   ") ", sep = "", collapse=""))
}

# конструирование ограничений запроса по данным фильтров
buildReqLimits <- function(begin, end, regions, prefixes, channels) {
  # базисная SQL конструкция для ограничения дат ----
  # limit_dates <- paste0(" toDate(begin) >= toDate('", begin, "') AND toDate(end) <= toDate('", end, "') ")
  limit_dates <- paste0(" date >= '", begin, "' AND date <= '", end, "' ")
  
  # добавочная SQL конструкция для ограничения регионов -----
  limit_regions <- 

  paste0(limit_dates, 
         " AND ", buildReqFilter("region", regions), 
         " AND ", buildReqFilter("prefix", prefixes),
         " AND ", buildReqFilter("prefix", channels)
         )
}

# построение запроса для отчета 'Рейтинг по каналам' ----------------
buildReq <- function(begin, end, regions, segment="all"){
  # begin, end -- даты; 
  # regs -- вектор регионов, если NULL -- то все регионы (в т.ч. на этапе инициализации);
  # segment -- регион (строка), если "all" -- то все сегменты;
  # browser()
  
  limits <- buildReqLimits(begin, end, regions, segment)

  paste(
    "SELECT ",
    # 1. Название канала
    "channelId, ",
    # 2. Кол-во уникальных приставок по каналу
    "uniq(serial) AS unique_stb, ",
    # Кол-во уникальных приставок по всем каналам выбранных регионов
    "( SELECT uniq(serial) ",
    "  FROM genstates ",
    "  WHERE ", limits, 
    "  AND duration>5*60 AND duration <2*60*60 ", # указали жестко длительность, в секундах
    ") AS total_unique_stb, ",  
    # 4. Суммарное время просмотра всеми приставками, мин
    "sum(duration)/60 AS channel_duration, ",
    # 8. Кол-во событий просмотра
    "count() AS watch_events ",
    "FROM genstates ",
    # "SAMPLE 0.1 ",
    "WHERE ", limits,
    "AND duration>5*60 AND duration <2*60*60 ", # указали жестко длительность, в секундах
    "GROUP BY channelId", sep="")
}

