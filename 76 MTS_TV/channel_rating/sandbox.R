library(tidyverse)
library(lubridate)
library(magrittr)
library(stringi)
library(shiny)
library(DBI)
library(anytime)
library(tictoc)
library(profvis)
library(microbenchmark)
# library(debug)
# library(clickhouse)
source("clickhouse.R")


con <- dbConnect(clickhouse(), host="10.0.0.44", port=8123L, user="default", password="")

tt4 <- dbGetQuery(con, "SHOW TABLES")


# подгрузим таблицу преобразовани€ транслита в русские названи€ городов
cities_df <- req(
  dbGetQuery(con, "SELECT * FROM regnames")  %>%
    mutate_if(is.character, `Encoding<-`, "UTF-8"))

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
