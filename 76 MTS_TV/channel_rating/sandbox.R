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


# подгрузим таблицу преобразования транслита в русские названия городов
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
  "channelId, ",
  "uniq(serial) AS unique_tv_box, ",
  "sum(duration) AS total_duration, ",
  "uniq(serial) / count() AS ratio_tv_box, ",
  "sum(duration) / ",
  "( ",
  "   SELECT sum(duration) ",
  "   FROM genstates ",
  " ) AS ratio_duration, ",
  "avg(duration) AS mean_duration, ",
  "avg(duration) / uniq(serial) AS parameter3, ",
  "count(serial) AS total_tv_box ",
  "FROM genstates ",
  "WHERE toDate(begin) >= toDate('", begin, "') AND toDate(end) <= toDate('", end, "') AND ",
  "region IN (", plain_regs, ") ",
  "GROUP BY channelId", sep="")
}

regions <- c("Moskva", "Barnaul")

r <- buildReq(begin=today(), end=today()+days(1), regions)
tt <- dbGetQuery(con, r)



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
