library(tidyverse)
library(lubridate)
library(scales)
library(magrittr)
library(forcats)
library(ggrepel)
library(stringi)
library(shiny)
library(DBI)
library(RPostgreSQL)
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


source("clickhouse.R")
# getwd()
eval(parse("funcs.R", encoding="UTF-8"))

con <- dbConnect(clickhouse(), host="10.0.0.44", port=8123L, user="default", password="")

tt4 <- dbGetQuery(con, "SHOW TABLES")

# подгрузим ограниченный список городов
city_subset <- read_csv("region.csv")

# подгрузим таблицу преобразования транслита в русские названия городов
cities_df <- req(
  dbGetQuery(con, "SELECT * FROM regnames")  %>%
    mutate_if(is.character, `Encoding<-`, "UTF-8") %>%
    filter(translit %in% pull(city_subset)))

# а теперь из этого надо сделать list для листбокса. Имя -- русское название, значение -- транслит
# m <- cities_df %>% column_to_rownames(var="russian") %>% select(translit)
# в base R будет так:
# m <- as.list(cities_df$translit)
# names(m) <- cities_df$russian

regions <- c("Moskva", "Barnaul")

# r <- buildReq(begin=today(), end=today()+days(1), regions)
r <- buildReq(begin="2017-06-28", end="2017-06-30", interval=15,
              regions, segment="all")

r <- buildReqGetTop(begin="2017-06-28", end="2017-06-30", 
                    regions, segment="all")

df0 <- dbGetQuery(con, r)

# делаем проверку на состав вектора с регионами ---
v <- c(NA, "2")
any(is.na(v)) 
is.null(v)

# подрезали ТОП выборку
channels <- df0 %>% 
  filter(row_number()<=5) %>% 
  pull(channelId)

r <- buildReqDynamic(begin="2017-06-28", end="2017-06-30", 
                     regions, interval=60, channels=channels, segment="all")

df <- dbGetQuery(con, r) %>%
  # время смотрения, мин
  mutate(channel_duration=round(as.numeric(channel_duration), 0)) %>%
  # превращаем временной маркер в POSIX
  mutate(timegroup=anytime(timegroup, tz="Europe/Moscow"))


# нарисуем lineplot
# gp <- ggplot(df, aes(timegroup, watch_events, color=channelId)) +
g <- guide_legend("Каналы")
gp <- ggplot(df, aes(timegroup, channel_duration, color=channelId)) +
  # geom_line(lwd=1.2, alpha=0.5) +
  # geom_point(shape=21, size=4, alpha=0.5) +
  geom_area(aes(colour=channelId, fill=channelId), alpha=0.5, position="stack") +
  guides(colour=g, fill=g) +
  #scale_fill_discrete(name="Experimental\nCondition") +
  #scale_colour_discrete(name="colour") +
  # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
  # geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
  # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
  scale_x_datetime(labels=date_format(format="%d.%m.%y%n%H:%M", tz="Europe/Moscow")
                   #breaks=date_breaks("2 hours"), 
                   #minor_breaks=date_breaks("1 hours")
  ) +
  # scale_y_continuous(limits=c(0.9*min(df$watch_events), NA), oob=rescale_none) +
  # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
  # scale_y_log10() +
  theme_ipsum_rc() + 
  #  theme_ipsum_rc(base_size=publish_set[["base_size"]], 
  #                 axis_title_size=publish_set[["axis_title_size"]]) +  
  # theme(axis.text.x = element_text(angle=90)) +
  ylab("Количество событий") +
  xlab("Временной интервал") # +
# ggtitle("Топ N регионов", subtitle="По суммарному времени телесмотрения, мин") +
# coord_flip()

gp


stop()
# сделаем wide таблицу и проверим отображение с помощью DT ----------
df0 <- df %>%
  select(-timestamp, -channel_duration) %>%
  spread(timegroup, watch_events)

# взглянем на табличку
DT::datatable(df0,
              options=list(
                columnDefs=list(list(width="100px", targets="_all"),
                                list(className='dt-center', targets="_all")),
                autoWidth=TRUE,
                scrollX=TRUE
                )
              )

stop()

# нарисуем график (не более 50 столбиков) --------------------
reg_df <- df %>%
  filter(row_number() <=50)

gp <- ggplot(reg_df, aes(timegroup, watch_events)) +
  geom_line(color=brewer.pal(n=9, name="Blues")[4], lwd=2, alpha=0.5) +
  geom_point(color=brewer.pal(n=9, name="Blues")[4], shape=1, size=4, alpha=1) +
  # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
  # geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
  # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
  scale_x_datetime(labels=date_format(format="%d.%m.%y%n%H:%M", tz="Europe/Moscow"),
                   breaks=date_breaks("2 hours"), 
                   minor_breaks=date_breaks("1 hours")
  ) +
  # coord_cartesian(ylim=c(0.9*min(reg_df$watch_events), NA)) +
  scale_y_continuous(limits=c(0.9*min(reg_df$watch_events), NA), oob=rescale_none) +
  # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
  # scale_y_log10() +
  theme_ipsum_rc() + 
  #  theme_ipsum_rc(base_size=publish_set[["base_size"]], 
  #                 axis_title_size=publish_set[["axis_title_size"]]) +  
  # theme(axis.text.x = element_text(angle=90)) +
  ylab("Количество событий") +
  xlab("Временной интервал") # +
# ggtitle("Топ N регионов", subtitle="По суммарному времени телесмотрения, мин") +
# coord_flip()

gp


if(FALSE){
# изначальный отчет
gp <- ggplot(reg_df, aes(timegroup, watch_events)) +
  geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
  # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
  # geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
  # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
  scale_x_datetime(labels=date_format(format="%d.%m.%y%n%H:%M", tz="Europe/Moscow"),
                   breaks=date_breaks("2 hours"), 
                   minor_breaks=date_breaks("1 hours")
                   ) +
  # coord_cartesian(ylim=c(0.9*min(reg_df$watch_events), NA)) +
  scale_y_continuous(limits=c(0.98*min(reg_df$watch_events), NA), oob=rescale_none) +
  # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
  # scale_y_log10() +
  theme_ipsum_rc() +
  #  theme_ipsum_rc(base_size=publish_set[["base_size"]], 
  #                 axis_title_size=publish_set[["axis_title_size"]]) +  
  # theme(axis.text.x = element_text(angle=90)) +
  ylab("Количество событий") +
  xlab("Временной интервал") # +
  # ggtitle("Топ N регионов", subtitle="По суммарному времени телесмотрения, мин") +
  # coord_flip()

gp
}
stop()




  # 3. % уникальных приставок
  mutate(ratio_per_stb=round(unique_stb/total_unique_stb, 3)) %>%
  left_join(cities_df, by=c("region"="translit")) %>%
  select(-region) %>%
  select(region=russian, everything())

# рисуем графики --------------
# Топ N по времени телесмотрения
reg_df <- df %>%
  top_n(10, total_duration) %>%
  arrange(desc(total_duration)) %>%
  mutate(label=format(total_duration, big.mark=" "))    

gp <- ggplot(reg_df, aes(fct_reorder(as.factor(region), total_duration, .desc=FALSE), total_duration)) +
  geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
  # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
  geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
  # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
  # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
  scale_y_log10() +
#  theme_ipsum_rc(base_size=publish_set[["base_size"]], 
#                 axis_title_size=publish_set[["axis_title_size"]]) +  
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Время телесмотрения") +
  xlab("Регион") +
  ggtitle("Топ N регионов", subtitle="По суммарному времени телесмотрения, мин") +
  coord_flip() 

gp
