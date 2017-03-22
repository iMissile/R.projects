plotTop10Downlink <- function(df) {
  
  flog.info(paste0("top10 download plot: nrow = ", nrow(df)))
  if(nrow(df)==0) return(NULL)
  
  plot_df <- df %>%
    filter(direction=="down") %>%
    mutate(msisdn=as.factor(msisdn)) %>%
    group_by(msisdn) %>%
    summarise(volume=round(sum(bytes)/1024/1024, 1)) %>% # Перевели в Мб
    top_n(10, volume)

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
    filter(direction=="up") %>%
    mutate(msisdn=as.factor(msisdn)) %>%
    group_by(msisdn) %>%
    summarise(volume=round(sum(bytes)/1024, 1)) %>% # Перевели в Кб
    top_n(10, volume)
  
  gp <- ggplot(plot_df, aes(fct_reorder(msisdn, volume), volume)) + 
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    theme_ipsum_rc(base_size=16, axis_title_size=14) +
    xlab("MSISN") +
    ylab("Суммарный Uplink, Кb") +
    ggtitle("ТОП 10 публикующих") +
    coord_flip()
  
  gp
}