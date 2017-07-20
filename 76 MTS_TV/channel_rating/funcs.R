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

# построение запроса для отчета 'Рейтинг по каналам' ----------------
buildReq <- function(begin, end, regs){
  # bigin, end -- даты; regs -- вектор регионов
  plain_regs <- stri_join(regs %>% map_chr(~stri_join("'", .x, "'", sep="")), 
                          sep = " ", collapse=",")
  # cat(plain_regs)
  
  paste(
    "SELECT ",
    # 1. Название канала
    "channelId, ",
    # 2. Кол-во уникальных приставок по каналу
    "uniq(serial) AS unique_tvbox, ",
    # Кол-во уникальных приставок по всем каналам
    "( SELECT uniq(serial) ",
    "  FROM genstates ",
    "  WHERE toDate(begin) >= toDate('", begin, "') AND toDate(end) <= toDate('", end, "') AND region IN (", plain_regs, ") ",
    ") AS total_unique_tvbox, ",  
    # 4. Суммарное время просмотра всеми приставками, мин
    "sum(duration) AS channel_duration, ",
    # 8. Кол-во событий просмотра
    "count() AS watch_events ",
    "FROM genstates ",
    "WHERE toDate(begin) >= toDate('", begin, "') AND toDate(end) <= toDate('", end, "') AND region IN (", plain_regs, ") ",
    "GROUP BY channelId", sep="")
}

# построение гистограммы ТОП 10 по времени просмотра для отчета 'Рейтинг по каналам' ----------------
plotTop10Duration <- function(df){
  # выберем наиболее программы c позиции эфирного времени
  reg_df <- df %>%
    top_n(10, channel_duration) %>%
    arrange(desc(channel_duration))
  
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(channelId), channel_duration, .desc=FALSE), channel_duration)) +
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    geom_text(aes(label=format(channel_duration, big.mark=" ")), hjust=+1, colour="red") + # для вертикальных
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
    scale_y_log10() +
    theme_ipsum_rc(base_size=20, axis_title_size=18) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Суммарное количество минут") +
    xlab("Канал") +
    ggtitle("Топ 10 каналов", subtitle="По времени телесмотрения") +
    coord_flip() 
  
  gp
}

# построение гистограммы ТОП 10 по количеству уникальных приставок для отчета 'Рейтинг по каналам' ----------------
plotTop10Unique <- function(df){
  # выберем наиболее программы c позиции эфирного времени
  reg_df <- df %>%
    top_n(10, unique_tvbox) %>%
    arrange(desc(unique_tvbox))
  
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(channelId), unique_tvbox, .desc=FALSE), unique_tvbox)) +
    geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
    geom_text(aes(label=format(unique_tvbox, big.mark=" ")), hjust=+1, colour="red") + # для вертикальных
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
    scale_y_log10() +
    theme_ipsum_rc(base_size=20, axis_title_size=18) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Количество уникальных приставок") +
    xlab("Канал") +
    ggtitle("Топ 10 каналов", subtitle="По количеству уникальных приставок") +
    coord_flip() 
  
  gp
}

# Генерация word файла для выгрузки средcтвами officer -------------
gen_word_report <- function(raw_df, template_fname){
  # считаем данные для вставки -----------------------------------
  out_df <- raw_df[1:80, ] %>% select(everything())
  
  # создаем файл ------------------------------------------
  doc <- read_docx() %>% # read_docx(path="./TV_report_template.docx") %>%
    body_add_par(value='Первые 80 строк данных', style="heading 1") %>%
    body_add_table(value=out_df, style="table_template") %>% 
    body_add_par(value="ТОП 10 по времени просмотра", style="heading 2") %>%
    body_add_gg(value=plotTop10Duration(raw_df), style = "centered") %>%
    body_add_par(value="ТОП 10 по количеству уникальных приставок", style="heading 2") %>%
    body_add_gg(value=plotTop10Unique(raw_df), width=8, style="centered")
  
  doc
  
  }


