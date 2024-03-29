processxDR <- function(fname, progress, ...){
  # выносим процесс загрузки в отдельный файл для того, чтобы иметь возможность делать потом прогресс бар и логирование
  # cat(fname)
  # browser()
  flog.info(paste0("Processing xDR '", fname, "'"))
  text <- paste0("...", stri_sub(basename(fname), from=-26, to=-1))
  progress$inc(amount=1, detail=text)
  
  df <- read_delim(fname, delim=',')
  # problems(df)
  df
}


hgroup.enum <- function(date, hour_bin=NULL, min_bin=5){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется hour.bin
  # отсчет измерений идет с 0:00
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  # если hour.bin=NULL, то идет привязка к интервалам min.bin, заданном в минутах
  
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

getTimeframe <- function(days_back=7, days_forward=0){
  # если по каким-либо причинам наверху не определились с прогнозом (NA),
  # то полагаем что он есть и он равен базовому горизонту
  days_formard <- ifelse(is.na(days_forward), 0, days_forward)
  min_lim <- floor_date(now() - days(days_back), unit = "day")
  # поскольку будущее округляем вниз, то надо добавить еще сутки (+1)
  max_lim <- ceiling_date(now() + days(days_forward), unit = "day")
  timeframe <- c(min_lim, max_lim)
  
  timeframe
}

plotTop10Downlink <- function(df) {
  
  flog.info(paste0("top10 download plot: nrow = ", nrow(df)))
  if(nrow(df)==0) return(NULL)
  
  plot_df <- df %>%
    mutate(msisdn=fct_reorder(msisdn, volume)) %>%
    mutate(volume=round(volume/1024/1024, 1)) # Перевели в Мб

  gp <- ggplot(plot_df, aes(msisdn, volume)) + 
    geom_bar(fill=brewer.pal(n=9, name="Blues")[4], 
             alpha=0.5, stat="identity") +
    theme_ipsum_rc(base_size=16, axis_title_size=14) +
    xlab("MSISN") +
    ylab("Суммарный Downlink, Мб") +
    ggtitle("ТОП 10 скачивающих") +
    coord_flip()
  
  gp
}

plotTop10Uplink <- function(df) {

  if(nrow(df)==0) return(NULL)
  
  plot_df <- df %>%
    mutate(msisdn=fct_reorder(msisdn, volume)) %>%
    mutate(volume=round(volume/1024, 1)) # Перевели в Кб

  gp <- ggplot(plot_df, aes(msisdn, volume)) + 
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    theme_ipsum_rc(base_size=16, axis_title_size=14) +
    xlab("MSISN") +
    ylab("Суммарный Uplink, Кб") +
    ggtitle("ТОП 10 публикующих") +
    coord_flip()
  
  gp
}

plotFacetTraffic <- function(df, pal="Set1", wrap=FALSE, point_labes=FALSE) {

  gp <- ggplot(df, aes(timegroup, volume)) + 
    # geom_line(aes(colour=direction), alpha=0.4, lwd=1) +
    # geom_point(aes(colour=direction), alpha=0.4, shape=1, size=2) +
    geom_line(aes(y=global_meanr, group=direction), alpha=0.85, linetype="longdash", lwd=1, colour="black") +
    scale_y_continuous(trans='log10') +
    #scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
    #              labels=trans_format("log10", math_format(10^.x))) +
    #annotation_logticks() +
    theme_ipsum_rc(base_size=16, axis_title_size=14) +
    # theme_ipsum_rc(base_family="robotoC", base_size=16, axis_title_size=14) +
    xlab("Дата, время") +
    ylab("Суммарный объем данных, Mb") +
    ggtitle("Динамика трафика", subtitle="Черный пунктир -- скользящее среднее по всем площадкам")

  if(wrap){
    # делаем цветовую раскладку по направлению
    gp <- gp + 
      facet_wrap(~site, scales="free", nrow=2) +
      # http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
      # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
      scale_color_brewer(palette=pal,
                         name="Трафик",
                         breaks=c("up", "down"),
                         labels=c("Uplink", "Downlink")
      ) +
      geom_point(aes(colour=direction, shape=direction), alpha=0.85, size=3) +
      geom_line(aes(y=volume_meanr, colour=direction), alpha=0.85, lwd=1)
  }else{
    # делаем цветовую раскладку по площадкам
    # http://stackoverflow.com/questions/6910988/change-both-legend-titles-in-a-ggplot-with-two-legends
    gp <- gp + 
      scale_color_brewer(palette=pal) +
      labs(colour="Площадка", linetype="Трафик", shape="Направление") +
      geom_point(aes(colour=site, shape=direction), alpha=0.85, size=3) +
      geom_line(aes(y=volume_meanr, colour=site, linetype=direction), alpha=0.85, lwd=1)
  } 

  if(point_labes){
    # создадим подмножество элементов по которым расставим метки
    label_df <- df %>%
      mutate(deviation=abs(1-volume/volume_meanr)) %>%
      filter(deviation>.6)
    
    gp <- gp +
      # geom_text_repel(aes(label=volume)) +
      geom_label_repel(data=label_df,
                       aes(timegroup, volume, label=round(volume, 1)),
                       fontface = 'bold', # color = 'white',
                       box.padding = unit(0.35, "lines"),
                       point.padding = unit(0.5, "lines"),
                       segment.color = 'grey50'
      )
    }
    
  gp
}

plotHttpCategory <- function(df) {
  
  if(nrow(df)==0) return(NULL)
  
  plot_df <- df %>%
    mutate(volume=round(volume/1024/1024, 1)) # Перевели в Мб

  # browser()
  gp <- ggplot(plot_df, aes(fct_reorder(category, volume), volume)) + 
    scale_fill_brewer(palette="Set1") +
    geom_bar(aes(fill=direction), alpha=0.85, stat="identity", position="dodge") +
    theme_ipsum_rc(base_size=16, axis_title_size=14) +
    xlab("Категория") +
    ylab("Суммарный трафик, Мб") +
    ggtitle("Объем трафика по категориям")

  gp
}