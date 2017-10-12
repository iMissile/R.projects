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
  #' получаем всю историческую программу вещания из в PostgreSQL ---------------------

  # Connect to a specific postgres database
  dw <- config::get("postgres")

  # dbConnect из RPostgreSQL
  conn <- dbConnect(dbDriver(dw$driver),
                    host = dw$host,
                    user = dw$uid,
                    password = dw$pwd,
                    port = dw$port,
                    dbname = dw$database
  )
  
  # принудительно загоняем кодировку сгруженных данных в unicode
  rs <- dbSendQuery(conn, "SELECT program_id, 
                          channel_id, 
                          duration/60/60 AS program_duration, 
                          title AS program_title,
                          extract(epoch FROM start_time) AS program_start_time,
                    extract(epoch FROM created_at) AS created_at FROM programs;")
  df <- dbFetch(rs)
  dbDisconnect(conn)
  
  # browser()
  df %<>%
    as_tibble() %>%
    mutate_if(is.character, `Encoding<-`, "UTF-8") %>%
    mutate(program_start_time=anytime(as.numeric(program_start_time), tz="UTC")) %>%
    mutate(created_at=anytime(as.numeric(created_at), tz="UTC")) %>%
    # Продолжительность программы, мин
    mutate(program_duration=round(program_duration, 0))
    
}

buildReqFilter <- function(field, conditions, add=TRUE){
  # потенциально надо проверять условия еще на NA, character(0)
  ifelse(((length(conditions) == 0) && (typeof(conditions) == "character")) || 
           is.null(conditions) || conditions=="all", 
         " ",
         stri_join(ifelse(add, " AND ", " "), field, " IN (",
                   stri_join(conditions %>% map_chr(~stri_join("'", .x, "'", sep="")),
                             sep=" ", collapse=","), ") ", sep = "", collapse=""))
}

# конструирование ограничений запроса по данным фильтров
buildReqLimits <- function(begin, end, region=NULL, prefix=NULL, channel=NULL, event=NULL, 
                           segment=NULL, serial_mask=NULL) {
  # region, prefix, channel -- вектора
  # собираем общие условия в соотв. с фильтрами
  res <- paste0(paste0(" date >= '", begin, "' AND date <= '", end, "' "),
                # указали жестко длительность, в секундах
                paste0(" AND duration>0*60 AND duration <2*60*60 "),
                buildReqFilter("region", region, add=TRUE),
                buildReqFilter("prefix", prefix, add=TRUE),
                buildReqFilter("segment", segment, add=TRUE),
                buildReqFilter("channelId", channel, add=TRUE),
                buildReqFilter("switchEvent", event, add=TRUE),
                ifelse(serial_mask=="", "", paste0(" AND like(serial, '%", serial_mask, "%') "))
  )   
}

# построение запроса для отчета 'Статистика по телепередачам' ----------------
buildReqStep1 <- function(db_table, begin, end, region=NULL, interval=60, channel=NULL, segment="all"){
  #' считаем ТОП 20 передач по общему времени смотрения при заданных фильтрах
  # begin, end -- даты; 
  # interval -- временной интервал агрегации, в минутах
  # channel -- вектор каналов
  # region -- вектор регионов, если NULL -- то все регионы (в т.ч. на этапе инициализации);
  # segment -- регион (строка), если "all" -- то все сегменты;
  # browser()
  where_string <- buildReqLimits(begin=begin, end=end, region=region, channel=channel, segment=segment)

  paste(
    "SELECT ",
    # 1. Название канала
    "programId,",
    # 2. Кол-во уникальных приставок по каналу
    "uniq(serial) AS unique_stb,",
    # Кол-во уникальных приставок по всем каналам выбранных регионов
    "(SELECT uniq(serial)", "FROM", db_table, "WHERE", where_string, ") AS total_unique_stb,",  
    # 4. Суммарное время просмотра всеми приставками, мин
    "sum(duration)/60 AS program_watch_time,",
    # 8. Кол-во событий просмотра
    "count() AS watch_events",
    "FROM", db_table, 
    # "SAMPLE 0.1",
    "WHERE",  where_string,
    "AND (programId != 'undefined') ",
    "GROUP BY programId ",
    "ORDER BY program_watch_time DESC",
    "LIMIT 20", sep=" ")
}

# Генерация word файла для выгрузки средcтвами officer -------------
gen_word_report <- function(df, template_fname, dict){
  # считаем данные для вставки -----------------------------------
  n_out <- ifelse(nrow(df)<80, nrow(df), 80)
  out_df <- df %>% 
    filter(row_number() < n_out)

  flog.info(paste0("Word report generation under ", Sys.info()["sysname"]))
  flog.info(paste0("Internal df column names are: ", names(out_df)))
  if (Sys.info()["sysname"] == "Linux") {
    # сделаем мэпинг русских имен колонок и подсказок
    colnames_df <- tibble(internal_name=names(out_df)) %>%
      left_join(dict, by=c("internal_name"))
    names(out_df) <- colnames_df$human_name_rus
  }
  
  target <- "word_A4"
  # создаем файл ------------------------------------------
  doc <- read_docx() %>% # read_docx(path="./TV_report_template.docx") %>%
    body_add_par(value=paste0("Первые ", n_out, " строк данных"), style="heading 1") %>%
    body_add_table(value=out_df, style="table_template") %>% 
    body_add_par(value="ТОП 10 по времени просмотра", style="heading 2") %>%
    body_add_gg(value=plotAreaplotActivity(df, target), style = "centered") %>%
    body_add_par(value="ТОП 10 по количеству уникальных приставок", style="heading 2") %>%
    body_add_gg(value=plotLineplotActivity(df, target), style="centered")
  
  doc
  
  }
