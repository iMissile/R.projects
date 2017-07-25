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

# построение запроса для отчета 'Активность пользователей по регионам' ----------------
buildReq <- function(begin, end, regions, segment="all"){
  # begin, end -- даты; 
  # regs -- вектор регионов, если NULL -- то все регионы (в т.ч. на этапе инициализации);
  # segment -- регион (строка), если "all" -- то все сегменты;
  # browser()
  
  # базисная SQL конструкция для ограничения дат
  limit_dates <- paste0(" toDate(begin) >= toDate('", begin, "') AND toDate(end) <= toDate('", end, "') ")
  
  # добавочная SQL конструкция для ограничения регионов
  
  limit_regions <- ifelse(is.null(regions), " ",
                          stri_join(" AND region IN (", 
                                    stri_join(regions %>% map_chr(~stri_join("'", .x, "'", sep="")),
                                              sep = " ", collapse=","),
                                    ") ", sep = "", collapse=""))

  # добавочная SQL конструкция для ограничения сегментов
  limit_segments <- ifelse(segment=="all", " ", 
                            stri_join(" AND segment IN (", 
                             stri_join(segment %>% map_chr(~stri_join("'", .x, "'", sep="")),
                                       sep = " ", collapse=","),
                             ") ", sep = "", collapse=""))
  
  paste(
    "SELECT ",
    # 1. Регион
    "region, ",
    # 2. Кол-во уникальных приставок по каналу
    "uniq(serial) AS unique_stb, ",
    # Кол-во уникальных приставок по всем каналам выбранных регионов
    "( SELECT uniq(serial) ",
    "  FROM genstates ",
    "  WHERE ", limit_dates, limit_regions, limit_segments, 
    "  AND duration>5*60 AND duration <2*60*60 ", # указали жестко длительность, в секундах
    ") AS total_unique_stb, ",  
    # 4. Суммарное время просмотра всеми приставками, мин
    "sum(duration)/60 AS total_duration, ",
    # 8. Кол-во событий просмотра
    "count() AS watch_events ",
    "FROM genstates ",
    # "SAMPLE 0.1 ",
    "WHERE ", limit_dates, limit_regions, limit_segments,
    "AND duration>5*60 AND duration <2*60*60 ", # указали жестко длительность, в секундах
    "GROUP BY region", sep="")
}

# построение гистограммы ТОП N по времени телесмотрения ----------------
# для отчета 'Рейтинг пользователей по регионам' 
plotTop10Duration <- function(df, publish_set, ntop=10){
  
  flog.info(paste0("publish_set is ", capture.output(str(publish_set))))
  # выберем наиболее программы c позиции эфирного времени
  reg_df <- df %>%
    top_n(ntop, total_duration) %>%
    # может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
    sample_n(ntop) %>%
    arrange(desc(total_duration)) %>%
    mutate(label=format(total_duration, big.mark=" ")) 

  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(region), total_duration, .desc=FALSE), total_duration)) +
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
    ylab("Время телесмотрения") +
    xlab("Регион") +
    ggtitle("Топ N регионов", subtitle="По суммарному времени телесмотрения, мин") +
    coord_flip() 
  
  gp
}

# построение гистограммы ТОП 10 по количеству уникальных приставок для отчета 'Рейтинг по каналам' ----------------
plotTop10STB <- function(df, publish_set, ntop=10){
  
  flog.info(paste0("publish_set is ", capture.output(str(publish_set))))
  # выберем наиболее программы c позиции эфирного времени
  reg_df <- df %>%
    top_n(ntop, unique_stb) %>%
    # может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
    sample_n(ntop) %>%
    arrange(desc(unique_stb)) %>%
    mutate(label=format(unique_stb, big.mark=" "))    
  
  # browser()
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(region), unique_stb, .desc=FALSE), unique_stb)) +
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
    ylab("Количество приставок") +
    xlab("Регион") +
    ggtitle("Топ N регионов", subtitle="По количеству приставок") +
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
  out_df <- df[1:80, ]

  flog.info(paste0("Word Report generation under ", Sys.info()["sysname"]))
  if (Sys.info()["sysname"] == "Linux") {
    out_df %<>% colNamesToRus()
  }
  
  # создаем файл ------------------------------------------
  doc <- read_docx() %>% # read_docx(path="./TV_report_template.docx") %>%
    body_add_par(value='Первые 80 строк данных', style="heading 1") %>%
    body_add_table(value=out_df, style="table_template") %>% 
    body_add_par(value="ТОП 10 по времени просмотра", style="heading 2") %>%
    body_add_gg(value=plotTop10Duration(df, publish_set=publish_set), style = "centered") %>%
    body_add_par(value="ТОП 10 по количеству уникальных приставок", style="heading 2") %>%
    body_add_gg(value=plotTop10STB(df, publish_set=publish_set), style="centered")
  
  doc
  
  }

# Локализация названий колонок в датасете --------------
colNamesToRus <- function(df){
  # используется исключительно перед выводом
  # локализовано, чтобы гибко подстраивать под возможные пожелания
  # browser()
  df %>% select("регион"=region,
                "кол-во уник. STB"=unique_stb,
                "всего уник. STB"=total_unique_stb,
                "суммарное время, мин"=total_duration,
                "кол-во просмотров"=watch_events, 
                "% уник. STB"=stb_ratio
                )
  
}