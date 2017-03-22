plotTop10 <- function(df) {
  
  group_df <- df %>%
    mutate(msisdn=as.factor(msisdn)) %>%
    group_by(msisdn) %>%
    summarise(user_recs=n(), 
              uplink_Kb=round(sum(uplink_bytes)/1024, 1), 
              downlink_Kb=round(sum(downlink_bytes)/1024, 1)) %>%
    arrange(desc(user_recs))
  
  plot_df <- group_df %>%
    top_n(10, downlink_Kb) %>%
    mutate(downlink_Mb=downlink_Kb/1024) %>%
    arrange(desc(downlink_Kb))
  
  gp <- ggplot(plot_df, aes(fct_reorder(msisdn, downlink_Mb), downlink_Mb)) + 
    geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
    theme_ipsum_rc(base_size=16, axis_title_size=14) +
    xlab("'Как бы' MSISN") +
    ylab("Суммарный Downlink, Mb") +
    coord_flip()
  
  gp
}