library(tidyverse)
library(lubridate)
library(magrittr)
library(forcats)
library(stringi)
library(shiny)
library(DBI)
library(anytime)
library(tictoc)
library(profvis)
library(microbenchmark)
library(Cairo)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
# library(debug)
# library(clickhouse)
source("clickhouse.R")


con <- dbConnect(clickhouse(), host="10.0.0.44", port=8123L, user="default", password="")

tt4 <- dbGetQuery(con, "SHOW TABLES")


# подгрузим ограниченный список городов
city_subset <- read_csv("region.csv")

# подгрузим таблицу преобразовани€ транслита в русские названи€ городов
cities_df <- req(
  dbGetQuery(con, "SELECT * FROM regnames")  %>%
    mutate_if(is.character, `Encoding<-`, "UTF-8") %>%
    filter(translit %in% pull(city_subset)))

# а теперь из этого надо сделать list дл€ листбокса. »м€ -- русское название, значение -- транслит
# m <- cities_df %>% column_to_rownames(var="russian") %>% select(translit)
# в base R будет так:
# m <- as.list(cities_df$translit)
# names(m) <- cities_df$russian

buildReq <- function(begin, end, regs){
  # bigin, end -- даты; regs -- вектор регионов
  plain_regs <- stri_join(regions %>% map_chr(~stri_join("'", .x, "'", sep="")), 
                          sep = " ", collapse=",")
  cat(plain_regs)
  
  paste(
  "SELECT ",
  # 1. Ќазвание канала
  "channelId, ",
  # 2.  ол-во уникальных приставок по каналу
  "uniq(serial) AS unique_tvbox, ",
  #  ол-во уникальных приставок по всем каналам
  "( SELECT uniq(serial) ",
  "  FROM genstates ",
  "  WHERE toDate(begin) >= toDate('", begin, "') AND toDate(end) <= toDate('", end, "') AND region IN (", plain_regs, ") ",
  ") AS total_unique_tvbox, ",  
  # 4. —уммарное врем€ просмотра всеми приставками, мин
  "sum(duration) AS channel_duration, ",
  # 8.  ол-во событий просмотра
  "count() AS watch_events ",
  "FROM genstates ",
  "WHERE toDate(begin) >= toDate('", begin, "') AND toDate(end) <= toDate('", end, "') AND region IN (", plain_regs, ") ",
  "GROUP BY channelId", sep="")
}

regions <- c("Moskva", "Barnaul")

r <- buildReq(begin=today(), end=today()+days(1), regions)
df <- dbGetQuery(con, r) %>%
  # 6. —реднее врем€ просмотра, мин
  mutate(mean_duration=channel_duration/watch_events) %>%
  # 3. % уникальных приставок
  mutate(ratio_per_tv_box=unique_tvbox/total_unique_tvbox) %>%
  # 5. % времени просмотра
  mutate(watch_ratio=channel_duration/sum(channel_duration)) %>%
  # 7. —реднее суммарное врем€ просмотра одной приставкой за период, мин
  mutate(duration_per_tvbox=channel_duration/unique_tvbox)




# считаем данные дл€ вставки -----------------------------------
# выберем наиболее программы c позиции эфирного времени
reg_df <- df %>%
  top_n(10, channel_duration) %>%
  arrange(desc(channel_duration))

gp <- ggplot(reg_df, aes(fct_reorder(as.factor(channelId), channel_duration, .desc=FALSE), channel_duration)) +
  geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
  geom_text(aes(label=format(channel_duration, big.mark=" ")), hjust=+1, colour="red") + # дл€ вертикальных
  # scale_x_discrete("ѕередача", breaks=df2$order, labels=df2$channelId) +
  scale_y_log10() +
  theme_ipsum_rc(base_size=14, axis_title_size=12) +  
  theme(axis.text.x = element_text(angle=90)) +
  ylab("—уммарное количество минут") +
  xlab(" анал") +
  ggtitle("—татистика телесмотрени€", subtitle="“оп 10 каналов") +
  coord_flip()  



format(reg_df$channel_duration, big.mark=" ")

# stri_join(regions %>% map_chr(~stri_join("-+", .x, "+-", sep=",")), sep = " ", collapse=" ")
stri_join(regions %>% map_chr(~stri_join("'", .x, "'", sep="")), sep = " ", collapse=",")

if (TRUE){
# протестируем работу с временем
e <- now()
b <- now() - days(3)

e-b
m <- interval (b, e)
m/days(1)

# ---
e <- today()
b <- today() - days(3)

e-b
m <- interval (b, e)
m/days(1)



today<-mdy(08312015)
dob<-mdy(09071982)

interval(dob, today) / years(1)
}
