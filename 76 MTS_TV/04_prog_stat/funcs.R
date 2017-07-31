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


getTvProgramm <- function() {
  #' получаем всю испторическую программу вещания из в PostgreSQL ---------------------

  # Connect to a specific postgres database
  if (Sys.info()["sysname"] == "Windows") {
    dw <- config::get("media-tel")
  }else{
    dw <- config::get("cti")
  }
  
  # dbConnect из RPostgreSQL
  con <- dbConnect(dbDriver(dw$driver),
                   host = dw$host,
                   user = dw$uid,
                   password = dw$pwd,
                   port = dw$port,
                   dbname = dw$database
  )
  
  # принудительно загоняем кодировку сгруженных данных в unicode
  rs <- dbSendQuery(con, "SELECT program_id, 
                          channel_id, 
                          duration/60/60 AS program_duration, 
                          title AS program_title,
                          extract(epoch FROM start_time) AS program_start_time,
                    extract(epoch FROM created_at) AS created_at FROM programs;")
  df <- dbFetch(rs)
  dbDisconnect(con)
  
  # browser()
  df %<>%
    as_tibble() %>%
    mutate_if(is.character, `Encoding<-`, "UTF-8") %>%
    mutate(program_start_time=anytime(as.numeric(program_start_time), tz="Europe/Moscow")) %>%
    mutate(created_at=anytime(as.numeric(created_at), tz="Europe/Moscow")) %>%
    # Продолжительность программы, мин
    mutate(program_duration=round(program_duration, 0))
    
}

# конструирование ограничений запроса по данным фильтров
buildReqLimits <- function(begin, end, regions, segment) {
  # базисная SQL конструкция для ограничения дат ----
  # limit_dates <- paste0(" toDate(begin) >= toDate('", begin, "') AND toDate(end) <= toDate('", end, "') ")
  limit_dates <- paste0(" date >= '", begin, "' AND date <= '", end, "' ")
  
  # добавочная SQL конструкция для ограничения регионов -----
  limit_regions <- ifelse(is.null(regions), " ",
                          stri_join(" AND region IN (", 
                                    stri_join(regions %>% map_chr(~stri_join("'", .x, "'", sep="")),
                                              sep = " ", collapse=","),
                                    ") ", sep = "", collapse=""))
  
  # добавочная SQL конструкция для ограничения сегментов -----
  limit_segments <- ifelse(segment=="all", " ", 
                           stri_join(" AND segment IN (", 
                                     stri_join(segment %>% map_chr(~stri_join("'", .x, "'", sep="")),
                                               sep = " ", collapse=","),
                                     ") ", sep = "", collapse=""))
  
  paste0(limit_dates, limit_regions, limit_segments)
}

# построение запроса для отчета 'Статистика по телепередачам' ----------------
buildReqStep1 <- function(begin, end, regions=NULL, interval=60, channels=NULL, segment="all"){
  #' считаем ТОП 20 передча по общему времени смотрения при заданных фильтрах
  # begin, end -- даты; 
  # interval -- временной интервал агрегации, в минутах
  # channels -- вектор каналов
  # regions -- вектор регионов, если NULL -- то все регионы (в т.ч. на этапе инициализации);
  # segment -- регион (строка), если "all" -- то все сегменты;
  # browser()
  
  limits <- buildReqLimits(begin, end, regions, segment)
  # добавочная SQL конструкция для ограничения каналов -----
  # прежде чем строить мы должны действительно убедиться, что мы получили вектор строк в качестве channels
  
  limit_channels <- ifelse(is.null(channels) | any(is.na(channels)) | identical(channels, character(0)), " ",
                          stri_join(" AND channelId IN (", 
                                    stri_join(channels %>% map_chr(~stri_join("'", .x, "'", sep="")),
                                              sep = " ", collapse=","),
                                    ") ", sep = "", collapse=""))

  paste(
    "SELECT ",
    # 1. Название канала
    "programId, ",
    # 2. Кол-во уникальных приставок по каналу
    "uniq(serial) AS unique_stb, ",
    # Кол-во уникальных приставок по всем каналам выбранных регионов
    "( SELECT uniq(serial) ",
    "  FROM genstates_10m ",
    "  WHERE ", limits, 
    "  AND duration>5*60 AND duration <2*60*60 ", # указали жестко длительность, в секундах
    ") AS total_unique_stb, ",  
    # 4. Суммарное время просмотра всеми приставками, мин
    "sum(duration)/60 AS program_watch_time, ",
    # 8. Кол-во событий просмотра
    "count() AS watch_events ",
    "FROM genstates_10m ",
    # "SAMPLE 0.1 ",
    "WHERE ", limits,
    "AND duration>5*60 AND duration <2*60*60 ", # указали жестко длительность, в секундах
    "AND (programId != 'undefined') ",
    "GROUP BY programId ",
    "ORDER BY program_watch_time DESC ",
    "LIMIT 10", sep="")
}


# Генерация word файла для выгрузки средcтвами officer -------------
gen_word_report <- function(df, template_fname, publish_set=NULL){
  if(is.na(publish_set)){
    flog.error("publish_set is NULL")
    return(NULL)
  }
  # считаем данные для вставки -----------------------------------
  n_out <- ifelse(nrow(df)<80, nrow(df), 80)
  out_df <- df %>% 
    filter(row_number() < n_out)

  flog.info(paste0("Word report generation under ", Sys.info()["sysname"]))
  if (Sys.info()["sysname"] == "Linux") {
    names_df <- getRusColnames(out_df)
    names(out_df) <- names_df$col_runame_office
  }
  
  # создаем файл ------------------------------------------
  doc <- read_docx() %>% # read_docx(path="./TV_report_template.docx") %>%
    body_add_par(value=paste0("Первые ", n_out, " строк данных"), style="heading 1") %>%
    body_add_table(value=out_df, style="table_template") %>% 
    body_add_par(value="ТОП 10 по времени просмотра", style="heading 2") %>%
    body_add_gg(value=plotAreaplotActivity(df, publish_set=publish_set), style = "centered") %>%
    body_add_par(value="ТОП 10 по количеству уникальных приставок", style="heading 2") %>%
    body_add_gg(value=plotLineplotActivity(df, publish_set=publish_set), style="centered")
  
  doc
  
  }

# Локализация названий колонок в датасете --------------
getRusColnames <- function(df) {
  colnames_df <- tribble(
    ~col_name, ~col_runame_screen, ~col_runame_office, ~col_label, 
    "region", "регион", "регион","подсказка (region)",
    "unique_stb", "кол-во уник. STB", "кол-во уник. STB", "подсказка (unique_stb)",
    "total_unique_stb", "всего уник. STB", "всего уник. STB", "подсказка (total_unique_stb)",
    "total_duration", "суммарное время, мин",	"суммарное время, мин",	"подсказка (total_duration)",
    "watch_events", "кол-во просмотров", "кол-во просмотров", "подсказка (watch_events)",
    "stb_ratio", "% уник. STB", "% уник. STB", "подсказка (stb_ratio)",
    "segment", "сегмент", "сегмент", "подсказка (segment)",
    "channelId", "канал (ID)", "канал  (ID)", "подсказка (channelId)",
    "channelName", "канал", "канал", "подсказка (channelName)",
    "channel_duration", "суммарное время, мин", "суммарное время, мин", "подсказка (channel_duration)",
    "mean_duration", "ср. время просмотра, мин", "ср. время просмотра, мин", "подсказка (mean_duration)",
    "watch_ratio", "% врем. просмотра", "% врем. просмотра", "подсказка (watch_ratio)",
    "duration_per_stb", "ср. время просм. 1 STB за период, мин", "ср. время просм. 1 STB за период, мин", "подсказка (duration_per_stb)",
    "date", "дата", "дата", "подсказка (date)",
    "timestamp", "время", "время", "подсказка (timestamp)",
    "timegroup", "группа", "группа", "подсказка (timegroup)",
    "program_duration", "длительность передачи", "длительность передачи", "подсказка (program_duration)",
    "program_title", "название передачи", "название передачи", "подсказка (program_title)",
    "program_start_time", "время начала передачи", "время начала передачи", "подсказка (program_start_time)",
    "program_watch_time", "общее время просмотра передачи, мин", "общее время просмотра передачи, мин", "подсказка (program_watch_time)",
    "mean_watch_time", "среднее время просмотра передачи, мин", "среднее время просмотра передачи, мин", "подсказка (mean_watch_time)"    
  )
    
  tibble(name=names(df)) %>%
    left_join(colnames_df, by=c("name"="col_name")) %>%
    # санация
    mutate(col_runame_screen=if_else(is.na(col_runame_screen), name, col_runame_screen)) %>%
    mutate(col_runame_office=if_else(is.na(col_runame_office), name, col_runame_office)) %>%
    mutate(col_label=if_else(is.na(col_label), name, col_label))
}