buildReqFilter <- function(field, conditions, add=TRUE){
  ifelse(is.null(conditions) || conditions=="all", " ",
         stri_join(ifelse(add, " AND ", " "), field, " IN (",
                   stri_join(conditions %>% map_chr(~stri_join("'", .x, "'", sep="")),
                                              sep = " ", collapse=","),
                                    ") ", sep = "", collapse=""))
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
                buildReqFilter("channelId", channel, add=TRUE),
                buildReqFilter("switchEvent", event, add=TRUE),
                ifelse(serial_mask=="", "", paste0(" AND like(serial, '%", serial_mask, "%') "))
  )   
}

buildReqDynamic <- function(db_table, begin, end, region=NULL, interval=60, channel=NULL, segment="all"){
  #`
  # begin, end -- даты; 
  # interval -- временной интервал агрегации, в минутах
  # channels -- вектор каналов
  # regions -- вектор регионов, если NULL -- то все регионы (в т.ч. на этапе инициализации);
  # segment -- регион (строка), если "all" -- то все сегменты;
  # browser()
  
  where_string <- buildReqLimits(begin=begin, end=end, region=region, channel=channel, segment=segment)

  paste(
    "SELECT ",
    # 1. временной интервал (как строка)
    "toDateTime(intDiv(toUInt32(begin), ", interval*60, ") *", interval*60, ") AS timestamp,",
    # 2. временной интервал (как целое)
    "toUInt32(timestamp) AS timegroup,",
    # 3. канал 
    "channelId,", 
    # 4. длительность телесмотрения в минутах и кол-во событий телесмотрения
    "sum(duration)/60 AS channel_duration, count() AS watch_events",
    "FROM", db_table, 
    # "SAMPLE 0.1",
    "WHERE", where_string, 
    "GROUP BY timestamp, channelId ",
    "ORDER BY timestamp DESC", sep=" ")
  }

plotAreaplotActivity <- function(df, publish_set, ntop=10){ 
  
  flog.info(paste0("publish_set is ", capture.output(str(publish_set))))
  
  # browser()
  reg_df <- df %>%
    rename(value=watch_events) # обезличили
#     group_by(channelName)
#     top_n(ntop, value) %>%
# # может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
#     filter(row_number()<=ntop) %>%
#     arrange(desc(value))

  g <- guide_legend("Каналы")
  gp <- ggplot(reg_df, aes(timegroup, value, color=channelName)) +
    # geom_line(lwd=1.2, alpha=0.5) +
    # geom_point(shape=21, size=4, alpha=0.5) +
    geom_area(aes(colour=channelName, fill=channelName), alpha=0.5, position="stack") +
    guides(colour=g, fill=g) +
    scale_x_datetime(labels=date_format(format="%d.%m.%y%n%H:%M", tz="UTC")) +
    theme_ipsum_rc(base_size=publish_set[["base_size"]], 
                   axis_title_size=publish_set[["axis_title_size"]]) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Количество событий") +
    xlab("Временной интервал")

  gp
  }

plotLineplotActivity <- function(df, publish_set, ntop=10){

  flog.info(paste0("publish_set is ", capture.output(str(publish_set))))
  
  # browser()
  reg_df <- df %>%
    rename(value=watch_events) # обезличили
  # top_n(ntop, value) %>%
  #   # может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
  #   filter(row_number()<=ntop) %>%
  #   arrange(desc(value))
  
  g <- guide_legend("Каналы")
  gp <- ggplot(reg_df, aes(timegroup, value, color=channelName)) +
    geom_line(lwd=1.2, alpha=0.5) +
    geom_point(shape=21, size=4, alpha=0.5) +
    guides(colour=g, fill=g) +
    # geom_area(aes(colour=channelName, fill=channelName), alpha=0.5, position="stack") +
    scale_x_datetime(labels=date_format(format="%d.%m.%y%n%H:%M", tz="UTC")) +
    theme_ipsum_rc(base_size=publish_set[["base_size"]], 
                   axis_title_size=publish_set[["axis_title_size"]]) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Количество событий") +
    xlab("Временной интервал")
  
  gp
}

# Генерация word файла для выгрузки средcтвами officer -------------
gen_word_report <- function(df, template_fname, publish_set=NULL, dict){
  if(is.na(publish_set)){
    flog.error("publish_set is NULL")
    return(NULL)
  }
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
