plotOutputScore <- function(df){
  gp <- ggplot(df, aes(x=docdate, y=actualvalue)) +
    geom_bar(aes(fill=status), stat="identity") +
    scale_fill_manual(
      values=c("FALSE"="brown1", "TRUE"="chartreuse4"),
      # breaks=c("4", "6", "8"),
      # ручное управление, сортировка по алфафиту
      labels=c("просадка", "в плане")
    ) +
    # нарисуем плановое значение точкой
    geom_point(aes(y=planvalue), colour="blue", shape=16, size=3) +
    geom_label(aes(label=label), position = position_stack(vjust = 0.5), 
               fill="white", colour="black", fontface="bold", hjust=.5) +
    facet_wrap(~material, nrow=1) +
    theme_ipsum_rc(base_size=20,
                   subtitle_size=14,
                   axis_title_size=18) +  
    # theme(axis.text.x = element_text(angle=90)) +
    ylab("Количетво, т") +
    xlab("Дата")
  
  gp  
}