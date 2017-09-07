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
  scale_fill_manual(
    values=c("FALSE"="brown1", "TRUE"="chartreuse4"),
    # breaks=c("4", "6", "8"),
    # ручное управление, сортировка по алфафиту
    labels=c("просадка", "в плане")
  )
  
  
