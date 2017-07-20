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

# построение запроса для отчета 'Рейтинг по каналам'
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

# Генерация word файла для выгрузки средcтвами ReporteRs ----------
gen_word_report_old <- function(raw_df, template_fname){
  doc <- docx(title='Рейтинг каналов', template=template_fname)
  # browser()
  
  doc <- addTitle(doc, 'Первые 80 строк данных', level = 1)
  out_df <- raw_df[1:80, ] %>% select(timestamp, region, programId, segment)
  doc <- addFlexTable(doc, vanilla.table(out_df))
  
  # выберем наиболее активные регионы c позиции эфирного времени
  reg_df <- raw_df %>%
    group_by(region) %>%
    summarise(duration=sum(duration), n=n()) %>%
    top_n(9, duration) %>%
    arrange(desc(duration))
  
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(region), duration, .desc=FALSE), duration)) +
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    #geom_text(aes(label=order), hjust=+0.5, colour="red") + # для вертикальных
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
    scale_y_log10() +
    theme_ipsum_rc(base_size=14, axis_title_size=12) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Суммарное количество минут") +
    xlab("Регион") +
    ggtitle("Статистика телесмотрения", subtitle="Топ 9 регионов") +
    coord_flip()  
  
  # browser()
  
  doc <- addParagraph(doc, value="Небольшая картинка")# , stylename = "Normal")
  doc <- addPlot(doc, function(){print(gp)}, width=6)
}

# Генерация word файла для выгрузки средcтвами officer -------------
gen_word_report <- function(raw_df, template_fname){
  # считаем данные для вставки -----------------------------------
  # выберем наиболее активные регионы c позиции эфирного времени
  reg_df <- raw_df %>%
    group_by(region) %>%
    summarise(duration=sum(duration), n=n()) %>%
    top_n(9, duration) %>%
    arrange(desc(duration))
  
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(region), duration, .desc=FALSE), duration)) +
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    #geom_text(aes(label=order), hjust=+0.5, colour="red") + # для вертикальных
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
    scale_y_log10() +
    theme_ipsum_rc(base_size=14, axis_title_size=12) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Суммарное количество минут") +
    xlab("Регион") +
    ggtitle("Статистика телесмотрения", subtitle="Топ 9 регионов") +
    coord_flip()  
  
  out_df <- raw_df[1:80, ] %>% select(timestamp, region, programId, segment)
  
  # создаем файл ------------------------------------------
  doc <- read_docx() %>% # read_docx(path="./TV_report_template.docx") %>%
    body_add_par(value='Первые 80 строк данных', style="heading 1") %>%
    body_add_table(value=out_df, style="table_template") %>% 
    body_add_par(value="Небольшая картинка", style="heading 1") %>%
    body_add_gg(value=gp, style = "centered")
  
  doc
  
  }


