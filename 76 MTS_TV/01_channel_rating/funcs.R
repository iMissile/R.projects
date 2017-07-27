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

# построение гистограммы ТОП 10 по времени просмотра для отчета 'Рейтинг по каналам' ----------------
plotTop10Duration <- function(df, publish_set, ntop=10){
  
  flog.info(paste0("publish_set is ", capture.output(str(publish_set))))
  # выберем наиболее программы c позиции эфирного времени
  reg_df <- df %>%
    top_n(ntop, channel_duration) %>%
# может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
    filter(row_number()<=ntop) %>%
    arrange(desc(channel_duration)) %>%
    mutate(label=format(channel_duration, big.mark=" "))

  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(channelId), channel_duration, .desc=FALSE), channel_duration)) +
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
    geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
    # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
    scale_y_log10() +
    theme_ipsum_rc(base_size=publish_set[["base_size"]],
                   subtitle_size=publish_set[["subtitle_size"]],
                   axis_title_size=publish_set[["axis_title_size"]]) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Суммарное количество минут") +
    xlab("Канал") +
    ggtitle("Топ 10 каналов", subtitle="По времени телесмотрения") +
    coord_flip() 
  
  gp
}

# построение гистограммы ТОП 10 по количеству уникальных приставок для отчета 'Рейтинг по каналам' ----------------
plotTop10STB <- function(df, publish_set, ntop=10){
  
  flog.info(paste0("publish_set is ", capture.output(str(publish_set))))
  # выберем наиболее программы c позиции эфирного времени
  reg_df <- df %>%
    top_n(ntop, unique_stb) %>%
    filter(row_number()<=ntop) %>% # на случай одинаковых значений
    arrange(desc(unique_stb)) %>%
    mutate(label=format(unique_stb, big.mark=" "))    
  
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(channelId), unique_stb, .desc=FALSE), unique_stb)) +
    geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
    # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
    geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
    # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
    scale_y_log10() +
    theme_ipsum_rc(base_size=publish_set[["base_size"]], 
                   subtitle_size=publish_set[["subtitle_size"]],
                   axis_title_size=publish_set[["axis_title_size"]]) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Количество уникальных приставок") +
    xlab("Канал") +
    ggtitle("Топ 10 каналов", subtitle="По количеству уникальных приставок") +
    coord_flip() 
  
  gp
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
    filter(row_number() < n_out) %>%
    select(-total_unique_stb)

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
    body_add_gg(value=plotTop10Duration(df, publish_set=publish_set), style = "centered") %>%
    body_add_par(value="ТОП 10 по количеству уникальных приставок", style="heading 2") %>%
    body_add_gg(value=plotTop10STB(df, publish_set=publish_set), style="centered")
  
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
    "channelId", "канал", "канал", "подсказка (channelId)",
    "channel_duration", "суммарное время, мин", "суммарное время, мин", "подсказка (channel_duration)",
    "mean_duration", "ср. время просмотра, мин", "ср. время просмотра, мин", "подсказка (mean_duration)",
    "watch_ratio", "% врем. просмотра", "% врем. просмотра", "подсказка (watch_ratio)",
    "duration_per_stb", "ср. время просм. 1 STB за период, мин", "ср. время просм. 1 STB за период, мин", "подсказка (duration_per_stb)",
    "date", "дата", "дата", "подсказка (date)",
    "timestamp", "время", "время", "подсказка (timestamp)",
    "timegroup", "группа", "группа", "подсказка (timegroup)"
  )
  
  tibble(name=names(df)) %>%
    left_join(colnames_df, by=c("name"="col_name")) %>%
    # санация
    mutate(col_runame_screen=if_else(is.na(col_runame_screen), name, col_runame_screen)) %>%
    mutate(col_runame_office=if_else(is.na(col_runame_office), name, col_runame_office)) %>%
    mutate(col_label=if_else(is.na(col_label), name, col_label))
}