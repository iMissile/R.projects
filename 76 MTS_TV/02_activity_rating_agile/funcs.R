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

# построение запроса для отчета 'Активность пользователей по регионам' ----------------
buildReq <- function(db_table, begin, end, region=NULL, segment="all"){
  # begin, end -- даты; 
  # region -- вектор регионов, если NULL -- то все регионы (в т.ч. на этапе инициализации);
  # segment -- регион (строка), если "all" -- то все сегменты;
  # browser()
  where_string <- buildReqLimits(begin=begin, end=end, region=region, segment=segment)

    paste(
    "SELECT",
    # 1. Регион
    "region,",
    # 2. Кол-во уникальных приставок по каналу
    "uniq(serial) AS unique_stb,",
    # Кол-во уникальных приставок по всем каналам выбранных регионов
    "(SELECT uniq(serial)", "FROM", db_table, "WHERE", where_string, ") AS total_unique_stb,",  
    # 4. Суммарное время просмотра всеми приставками, мин
    "sum(duration)/60 AS total_duration,",
    # 8. Кол-во событий просмотра
    "count() AS watch_events",
    "FROM", db_table, 
    # "SAMPLE 0.1",
    "WHERE",  where_string,
    "GROUP BY region", sep=" ")
}


buildReqTS <- function(db_table, begin, end, region=NULL, interval=60, segment="all"){
  # begin, end -- даты; 
  # interval -- временной интервал агрегации, в минутах
  # channels -- вектор каналов
  # regions -- вектор регионов, если NULL -- то все регионы (в т.ч. на этапе инициализации);
  # segment -- регион (строка), если "all" -- то все сегменты;
  where_string <- buildReqLimits(begin=begin, end=end, region=region, segment=segment)
  paste(
    "SELECT",
    "region,",
    # временной интервал (как строка)
    "toDateTime(intDiv(toUInt32(begin), ", interval*60, ") *", interval*60, ") AS timestamp,",
    # временной интервал (как целое)
    "toUInt32(timestamp) AS timegroup,",
    # кол-во уникальных приставок по региону
    "uniq(serial) AS unique_stb,",    
    # Кол-во уникальных приставок по всем каналам выбранных регионов
    "(SELECT uniq(serial)", "FROM", db_table, "WHERE", where_string, ") AS total_unique_stb,",  
    # длительность телесмотрения в минутах и кол-во событий телесмотрения
    "sum(duration)/60 AS total_duration, count() AS watch_events",
    "FROM", db_table, 
    # "SAMPLE 0.1",
    "WHERE", where_string, 
    "GROUP BY timestamp, region ",
    "ORDER BY timestamp  DESC", sep=" ")
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

# построение time-series отчета по выбранному региону
plotRegionHistory <- function(df, var_name, plot_type, target){
  tic()
  reg_df <- df %>%
    rename(value=watch_events) # обезличили

  g <- guide_legend("Регион")
  # browser()
  gp <- ggplot(df, aes_string("timegroup", var_name, fill="region")) +
    guides(colour=g, fill=g) +
    # geom_area(aes(colour=channelName, fill=channelName), alpha=0.5, position="stack") +
    scale_color_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2") +
    scale_x_datetime(labels=date_format(format="%d.%m.%y%n%H:%M", tz="UTC")) +
    theme_dvt(target) +  
    # theme(axis.text.x = element_text(angle=90)) +
    ylab("Метрика") +
    xlab("Временной интервал")
  
  if(plot_type=='lineplot'){
    gp <- gp +
      geom_line(lwd=1.2, alpha=0.3, aes_string(colour="region")) +
      geom_point(shape=21, size=4, alpha=0.5)
  }
  if(plot_type=='barplot'){
    gp <- gp +
      geom_bar(alpha=0.8, stat="identity", position="dodge")
  }
  
  flog.info(paste0("Building Time-Series Plot: ", capture.output(toc())))
  gp
}

# построение гистограммы ТОП N по времени телесмотрения ----------------
# для отчета 'Рейтинг пользователей по регионам' 
plotTop10Duration <- function(df, target, ntop=10){
  # выберем наиболее популярные программы c позиции эфирного времени
  tic()
  reg_df <- df %>%
    top_n(ntop, total_duration) %>%
    # может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
    filter(row_number()<=ntop) %>%
    arrange(desc(total_duration)) %>%
    mutate(label=format(total_duration, big.mark=" ")) 

  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(region), total_duration, .desc=FALSE), total_duration)) +
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
    geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
    # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelName) +
    scale_y_log10() +
    theme_dvt(target) +
    # theme(axis.text.x = element_text(angle=90)) +
    ylab("Время телесмотрения") +
    xlab("Регион") +
    ggtitle("Топ N регионов", subtitle="По суммарному времени телесмотрения, мин") +
    coord_flip() 
  
  flog.info(paste0("Building Top10Duration: ", capture.output(toc())))
  gp
}

# построение гистограммы ТОП 10 по количеству уникальных приставок для отчета 'Рейтинг по каналам' ----------------
plotTop10STB <- function(df, target, ntop=10){
  # выберем наиболее популярные программы c позиции эфирного времени
  tic()
  reg_df <- df %>%
    top_n(ntop, unique_stb) %>%
    filter(row_number()<=ntop) %>% # на случай одинаковых значений
    arrange(desc(unique_stb)) %>%
    mutate(label=format(unique_stb, big.mark=" "))    
  
  # browser()
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(region), unique_stb, .desc=FALSE), unique_stb)) +
    geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
    # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
    geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
    # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelName) +
    scale_y_log10() +
    theme_dvt(target) + 
    ylab("Количество приставок") +
    xlab("Регион") +
    ggtitle("Топ N регионов", subtitle="По количеству приставок") +
    coord_flip() 
  
  flog.info(paste0("Building Top10STB: ", capture.output(toc())))
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
