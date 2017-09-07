plotOutputScore <- function(df){
  gp <- ggplot(df, aes(x=docdate, y=actualvalue)) +
    geom_bar(aes(fill=status, alpha=opacity), stat="identity") +
    scale_fill_manual(
      values=c("FALSE"="brown1", "TRUE"="chartreuse4"),
      # breaks=c("4", "6", "8"),
      # ручное управление, сортировка по алфафиту
      labels=c("просадка", "в плане")
    ) +
    scale_alpha_manual(values=c("Hide"=0.3, "Mark"=1)) +
    # нарисуем плановое значение точкой
    geom_point(aes(y=planvalue), colour="goldenrod", shape=4, size=3, stroke=3) +
    geom_label(aes(label=label_up), position = position_stack(vjust = 0.6), 
               fill="white", colour="black", fontface="bold", hjust=.5) +
    geom_label(aes(label=label_down), position = position_stack(vjust = 0.4), 
               fill="white", colour="black", fontface="bold", hjust=.5) +
    # facet_wrap(~composed_kpi, nrow=1, scales="free_y")
    # facet_wrap(~material+kpi, nrow=1, scales="free_y")
    facet_grid(kpi~material, scales="free_y") +
    scale_y_log10(
      # breaks = scales::trans_breaks("log10", function(x) 10^x)
      # labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    annotation_logticks() +
    theme_ipsum_rc(base_size=20,
                   # plot_title_size=10,
                   subtitle_size=18,
                   # caption_size=12,
                   strip_text_size=16, # заголовок в facet
                   axis_title_size=18) +  
    # theme(axis.text.x = element_text(angle=90)) +
    ylab("Количеcтво, т") +
    xlab("Дата")
  
  gp  
}


plotKPIts <- function(df){
  kpis_df <- df %>%
    mutate(status=ratio>=1)
  
  ggplot(kpis_df, aes(docdate, actualvalue)) +
    # "lightpink" 
    geom_area(aes(y=planvalue), fill="navajowhite", alpha=0.5) +
    geom_line(linetype=2, size=1) +
    geom_point(aes(fill=status), shape=21, size=3, stroke=1.5, alpha=0.5) +
    scale_fill_manual(
      values=c("FALSE"="brown1", "TRUE"="chartreuse4"),
      # breaks=c("4", "6", "8"),
      # ручное управление, сортировка по алфафиту
      labels=c("просадка", "в плане")
    )
}