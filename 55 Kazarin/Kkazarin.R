library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(magrittr)
library(purrr)
library(stringi)
library(stringr)
library(tibble)
library(readxl)
library(iterators)
library(foreach)
library(doParallel)
library(zoo)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
library(RColorBrewer)


source("common_funcs.R")


data_filename <- "./data/отчет с детализацией по ОКС (056-2000815, 022-2000791, 051-2002476).xlsx"


# df <- foreach(it = iter(sheets), .combine = rbind, .packages='readxl') %do% {
#   temp.df <- get_month_data(datafile, it) %>% mutate(month = it)
# 
#   temp.df
# }

df <- getOksData(data_filename)

df_gp <- df %>%
  group_by(year) %>%
  summarise(num=n(), pd=sum(pd_cost), smp=sum(smp_cost), plant=sum(plant_cost))

df_gp1 <- df_gp %>%
  select(-num) %>%
  gather(key=type, value=cost, -year) # превратили в long для отрисовки

df_gp2 <- df_gp %>%
  select(year, num)

xlims <- c(min(df_out$year)-1, max(df_out$year)+1)

gp1 <- ggplot(data = df_gp1, aes(x=year, y=cost, fill=type)) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_brewer(palette="Set1", direction=1, guide=guide_legend()) +
  geom_bar(stat="identity", position="dodge") +
  theme(legend.position="top") +
  #geom_point(size = 3, fill = "yellow", shape = 21, na.rm = TRUE) +    # White fill
  #geom_abline(intercept = 0, slope = 1, colour = "red") +
  scale_x_continuous(limits=xlims, breaks=seq(xlims[[1]], xlims[[2]], 1)) +
  labs(x="Год", y="Сметная стоимость\n по ПД, руб.")

gp1

gp2 <- ggplot(data = df_gp2, aes(x=year, y=num)) +
  theme_bw() +
  geom_point(size=3, fill="yellow", shape=21, na.rm=TRUE) +    # White fill
  geom_line(colour="red") +
  scale_x_continuous(limits=xlims, breaks=seq(xlims[[1]], xlims[[2]], 1)) +
  labs(x="Год", y="Количество ОКС, ед.")

gp2

stop()
# Table
gp3 <- tableGrob(head(spread(df_out, year, cost)))
grid.draw(gp3)

# рисуем графики один под другим с синхронизированными осями
grid.arrange(gp1, gp2, gp3, ncol = 1) # возвращаем ggplot
grid.newpage()
grid.draw(rbind.gtable(ggplotGrob(gp1), ggplotGrob(gp2), size = "max"))


# grid.table(df_gp)
grid.newpage()
grid.arrange(rbind.gtable(ggplotGrob(gp1), ggplotGrob(gp2), size = "max"), gp3, ncol = 1) # возвращаем ggplot



