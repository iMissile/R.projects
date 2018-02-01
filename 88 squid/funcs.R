#' Arrange time vector according equidistant time intervals
#'
#' There are two possible ways of arrangement: by hour's intervals OR by
#' minute's interval. Hour's arrangement has high priority. Hour arrangement
#' must be integer (1, 2, ...) or fractional part of hour (0.25 or 0.5 only)
#'
#' @param date POSIXct vector to arrange
#' @param hours_bin Duration (in hours) between arrangement points
#' @param mins_bin Duration (in minutes) between arrangement points
#' @export
hgroup.enum2 <- function(date, hours_bin=NULL, mins_bin=5){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется hour.bin
  # отсчет измерений идет с 0:00
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  # если hour.bin=NULL, то идет привязка к интервалам min.bin, заданном в минутах
  # необходим пакет lubridate
  
  tick_time <- date
  
  if (is.null(hours_bin)){
    # привязываем к минутным интервалам
    n <- floor(lubridate::minute(tick_time)/mins_bin)
    dt <- lubridate::floor_date(tick_time, unit="hour") + lubridate::minutes(n * mins_bin)
  }else{
    # привязываем к часовым интервалам
    if (hours_bin < 1 & !(hours_bin %in% c(0.25, 0.5))) hours_bin=1
    n <- floor((lubridate::hour(tick_time)*60 + lubridate::minute(tick_time))/ (hours_bin*60))
    dt <- lubridate::floor_date(tick_time, unit="day") + lubridate::minutes(n * hours_bin*60)
  }
  
  dt
}

loadSquidLog <- function(fname){
  checkmate::qassert(fname, "S=1")
  
  raw_df <- read_table2(fname, 
                        col_names=c("timestamp", "duration", "client_address", "result_codes", 
                                    "bytes", "request_method", "url", "user", "hierarcy_code", "type"),
                        col_types=("nicciccccc")
  )
  # browser()
  df0 <- raw_df %>%
    mutate_at(vars(timestamp), anytime, tz="Europe/Moscow") %>%
    mutate(url=stri_replace_all_regex(url, 
                                      pattern=c("^([a-z]*)://", "^www\\.", "([^/]+).+"),
                                      replacement=c("", "", "$1"),
                                      vectorize_all=FALSE)) %>%
    mutate_at(vars(client_address), as.factor)
  
  df0
}


#=============================================
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

#' Title
#'
#' @param df raw traffic dataframe limited by time
#' @param subtitle gplot subtitle
#'
#' @return
#' @export
#'
#' @examples
plotTopIpDownload <- function(df, subtitle) {
  flog.info(paste0("Top10 IP download plot: nrow = ", nrow(df)))
  if(nrow(df)==0) return(NULL)
  
  # -------------- нарисуем Top10 по IP за последние N минут ----
  df0 <- df %>%
    # mutate(timegroup=hgroup.enum(timestamp, mins_bin=60)) %>%
    # mutate(date=lubridate::date(timegroup)) %>%
    # filter(date==date(now()-days(1))) %>%
    # filter(timestamp > now()-days(2)) %>%
    # filter(between(timestamp, anytime("2017-09-01"), anytime("2017-10-01")))
    select(ip=client_address, bytes, url) %>%
    group_by(ip) %>%
    summarise(volume=round(sum(bytes)/1024/1024, 1)) %>% # Перевели в Мб
    top_n(10, volume) %>%
    # может возникнуть ситуация, когда все значения top_n одинаковы. тогда надо брать выборку
    arrange(desc(volume)) %>%
    filter(row_number()<=10) %>%
    # уберем ненужный дребезг, все кто скачали менее 10Мб -- в топку
    filter(!(volume<10 & row_number()>2)) %>%
    mutate(label=format(volume, big.mark=" ")) %>%
    mutate(ip=fct_reorder(ip, volume))
  
  gp <- ggplot(df0, aes(ip, volume)) + 
    geom_bar(fill=brewer.pal(n=9, name="Blues")[4], 
             alpha=0.5, stat="identity") +
    geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
    theme_ipsum_rc(base_size=16, axis_title_size=14, subtitle_size=13) +
    xlab("IP") +
    ylab("Суммарный Downlink, Мб") +
    ggtitle("ТОП 10 скачивающих", subtitle=subtitle) +
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

  # g <- guide_legend("Трафик")
  gp <- ggplot(df, aes(timegroup, volume)) + 
    # geom_line(aes(colour=direction), alpha=0.4, lwd=1) +
    # geom_point(aes(colour=direction), alpha=0.4, shape=1, size=2) +
    geom_line(aes(y=global_meanr, group=direction), alpha=0.85, linetype="longdash", lwd=1, colour="black") +
    scale_y_continuous(trans='log10') +
    # guides(colour=g, fill=g, shape=g) +
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
      scale_color_brewer(palette=pal
                         # name="Направление",
                         # breaks=c("up", "down")
                         #labels=c("Uplink", "Downlink")
      ) +
      # guides(colour=guide_legend("Направление"), size=guide_legend("title"),
      #        shape=guide_legend("Направление"), group=guide_legend("Направление")) +
      labs(colour="Направление", 
           color="Направление2",
           shape="Направление"
           ) +
      geom_point(aes(colour=direction, shape=direction), alpha=0.85, size=3) +
      geom_line(aes(y=volume_meanr, colour=direction), alpha=0.85, lwd=1)
  }else{
    # делаем цветовую раскладку по площадкам
    # http://stackoverflow.com/questions/6910988/change-both-legend-titles-in-a-ggplot-with-two-legends
    gp <- gp + 
      scale_color_brewer(palette=pal) +
      # labs(colour="Площадка", linetype="Трафик", shape="Направление") +
      geom_point(aes(colour=site, shape=direction), alpha=0.85, size=3) +
      geom_line(aes(y=volume_meanr, colour=site, linetype=direction), alpha=0.85, lwd=1) +
      labs(colour="Площадка", linetype="Направление", shape="Направление")
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