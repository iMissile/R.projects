plotTop10Downlink <- function(df) {
  
  flog.info(paste0("top10 download plot: nrow = ", nrow(df)))
  if(nrow(df)==0) return(NULL)
  
  plot_df <- df %>%
    mutate(volume=round(volume/1024/1024, 1)) # Перевели в Мб

  gp <- ggplot(plot_df, aes(fct_reorder(msisdn, volume), volume)) + 
    geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
    theme_ipsum_rc(base_size=16, axis_title_size=14) +
    xlab("MSISN") +
    ylab("Суммарный Downlink, Mb") +
    ggtitle("ТОП 10 скачивающих") +
    coord_flip()
  
  gp
}

plotTop10Uplink <- function(df) {

  if(nrow(df)==0) return(NULL)
  
  plot_df <- df %>%
    mutate(volume=round(volume/1024, 1)) # Перевели в Кб

  gp <- ggplot(plot_df, aes(fct_reorder(msisdn, volume), volume)) + 
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    theme_ipsum_rc(base_size=16, axis_title_size=14) +
    xlab("MSISN") +
    ylab("Суммарный Uplink, Кb") +
    ggtitle("ТОП 10 публикующих") +
    coord_flip()
  
  gp
}

hgroup.enum <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  
  tick_time <- date
  if (time.bin < 1 & !(time.bin %in% c(0.25, 0.5))) time.bin=1
  n <- floor((hour(tick_time)*60 + minute(tick_time))/ (time.bin * 60))
  floor_date(tick_time, unit="day") + minutes(n * time.bin *60)
}

plotFacetTraffic <- function(df) {

  gp <- ggplot(df, aes(timegroup, volume)) + 
    # http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
    facet_wrap(~site, scales="free", nrow=2) +
    # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
      scale_color_brewer(palette="Set1",
                         name="Трафик",
                         breaks=c("up", "down"),
                         labels=c("Uplink", "Downlink")
      ) +
    # geom_line(aes(colour=direction), alpha=0.4, lwd=1) +
    # geom_point(aes(colour=direction), alpha=0.4, shape=1, size=2) +
    geom_line(aes(y=volume_meanr, colour=direction), alpha=0.6, lwd=1) +
    geom_point(aes(colour=direction), alpha=0.6, shape=1, size=3) +      
    scale_y_continuous(trans='log10') +
    #scale_y_log10(breaks=trans_breaks("log10", function(x) 10^x),
    #              labels=trans_format("log10", math_format(10^.x))) +
    #annotation_logticks() +
    theme_ipsum_rc(base_size=16, axis_title_size=14) +
    # theme_ipsum_rc(base_family="robotoC", base_size=16, axis_title_size=14) +
    xlab("Дата, время") +
    ylab("Суммарный объем данных, Mb")
  
  gp
}