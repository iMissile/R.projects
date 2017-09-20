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

# построение запроса для отчета 'Рейтинг по каналам' ----------------
buildReq <- function(db_table, begin, end, region=NULL, segment="all"){
  # begin, end -- даты; 
  # regs -- вектор регионов, если NULL -- то все регионы (в т.ч. на этапе инициализации);
  # segment -- регион (строка), если "all" -- то все сегменты;
  # browser()
  where_string <- buildReqLimits(begin=begin, end=end, region=region, segment=segment)

  paste(
    "SELECT ",
    # 1. Название канала
    "channelId,",
    # 2. Кол-во уникальных приставок по каналу
    "uniq(serial) AS unique_stb,",
    # Кол-во уникальных приставок по всем каналам выбранных регионов
    "(SELECT uniq(serial)", "FROM", db_table, "WHERE", where_string, ") AS total_unique_stb,",  
    # 4. Суммарное время просмотра всеми приставками, мин
    "sum(duration)/60 AS channel_duration,",
    # 8. Кол-во событий просмотра
    "count() AS watch_events",
    "FROM", db_table, 
    # "SAMPLE 0.1",
    "WHERE",  where_string,
    "GROUP BY channelId", sep=" ")
}

theme_dvt <- function(target="screen"){
  # создание параметров оформления для различных видов графиков (screen\publish) ------
  flog.info(paste0("Target is '", target, "'"))

  if(target=="screen"){
    ret <- theme_ipsum_rc(base_size=20, axis_title_size=18, subtitle_size=15)
  } else {
    if(target=="word_A4"){
      ret <- theme_ipsum_rc(base_size=14, axis_title_size=12, subtitle_size=11)
    } else {
      flog.error("Incorrect target, use default settings")
      ret <- theme_ipsum_rc()
    }}
  
  ret # + theme(axis.text.x = element_text(angle=90))
}

# построение гистограммы ТОП 10 по времени просмотра для отчета 'Рейтинг по каналам' ----------------
plotTop10Duration <- function(df, target, ntop=10){
  # выберем наиболее программы c позиции эфирного времени
  tic()
  reg_df <- df %>%
    top_n(ntop, channel_duration) %>%
# может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
    filter(row_number()<=ntop) %>%
    arrange(desc(channel_duration)) %>%
    mutate(label=format(channel_duration, big.mark=" "))

  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(channelName), channel_duration, .desc=FALSE), channel_duration)) +
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
    geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
    # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelName) +
    scale_y_log10() +
    theme_dvt(target) +
    ylab("Суммарное количество минут") +
    xlab("Канал") +
    ggtitle("Топ 10 каналов", subtitle="По времени телесмотрения") +
    coord_flip() 
  
  flog.info(paste0("Building Top10Duration Plot: ", capture.output(toc())))
  gp
}

# построение гистограммы ТОП 10 по количеству уникальных приставок для отчета 'Рейтинг по каналам' ----------------
plotTop10STB <- function(df, target, ntop=10){
  # выберем наиболее программы c позиции эфирного времени
  tic()
  reg_df <- df %>%
    top_n(ntop, unique_stb) %>%
    filter(row_number()<=ntop) %>% # на случай одинаковых значений
    arrange(desc(unique_stb)) %>%
    mutate(label=format(unique_stb, big.mark=" "))    
  
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(channelName), unique_stb, .desc=FALSE), unique_stb)) +
    geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
    # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
    geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
    # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelName) +
    scale_y_log10() +
    theme_dvt(target) + 
    ylab("Количество уникальных приставок") +
    xlab("Канал") +
    ggtitle("Топ 10 каналов", subtitle="По количеству уникальных приставок") +
    coord_flip() 
  
  flog.info(paste0("Building Top10STB Plot: ", capture.output(toc())))
  gp
}

# Генерация word файла для выгрузки средcтвами officer -------------
gen_word_report <- function(df, template_fname, dict){
  # считаем данные для вставки -----------------------------------
  n_out <- ifelse(nrow(df)<80, nrow(df), 80)
  out_df <- df %>% 
    filter(row_number() < n_out) %>%
    select(-total_unique_stb)

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
    body_add_gg(value=plotTop10Duration(df, target), style = "centered") %>%
    body_add_par(value="ТОП 10 по количеству уникальных приставок", style="heading 2") %>%
    body_add_gg(value=plotTop10STB(df, target), style="centered")
  
  doc
  
  }