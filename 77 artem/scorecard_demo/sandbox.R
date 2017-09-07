library(tidyverse)
library(lubridate)
library(scales)
library(magrittr)
library(forcats)
library(ggrepel)
library(stringi)
library(shiny)
library(DT)
library(formattable)
library(odbc)
# library(RSQLServer)
# library(DBI)
library(RODBC)
# library(RODBCDBI)
# library(RPostgreSQL)
library(anytime)
library(tictoc)
library(profvis)
library(microbenchmark)
library(Cairo)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
# library(debug)
library(config)

df <- readRDS("df.rds")


kpis_df <- df %>%
  filter(name %in% c("Выпуск ТК ПЗБМ ОБФ 1")) %>%
  mutate(status=ratio>=1)


ggplot(kpis_df, aes(docdate, actualvalue)) +
  # "lightpink" 
  geom_area(aes(y=planvalue), fill="navajowhite", alpha=0.5) +
  geom_line(linetype=2, size=1) +
  geom_point(aes(fill=status), shape=21, size=3, stroke=1.5, alpha=0.5) +
  # geom_label(aes(label=actualvalue)) +
  geom_label_repel(aes(label=actualvalue),
                   fontface = 'bold', # color = 'white',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.5, "lines"),
                   segment.color = 'grey50'
  ) +
  scale_fill_manual(
    values=c("FALSE"="brown1", "TRUE"="chartreuse4"),
    # breaks=c("4", "6", "8"),
    # ручное управление, сортировка по алфафиту
    labels=c("просадка", "в плане")
  ) +
  theme_ipsum_rc(base_size=20,
                 # plot_title_size=10,
                 subtitle_size=18,
                 # caption_size=12,
                 strip_text_size=16, # заголовок в facet
                 axis_title_size=18) +  
  # theme(axis.text.x = element_text(angle=90)) +
  ylab("Показатель") +
  xlab("Дата")  
  
  
