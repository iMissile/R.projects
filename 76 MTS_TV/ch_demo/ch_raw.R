library(tidyverse)
library(magrittr)
library(DBI)
library(anytime)
library(fasttime)
library(tictoc)
library(profvis)
library(microbenchmark)
# library(debug)
# library(clickhouse)
source("clickhouse.R")


# https://rviews.rstudio.com/2017/05/17/databases-using-r/
# https://github.com/yandex/clickhouse-odbc
# https://github.com/rstats-db/odbc


# https://github.com/hannesmuehleisen/clickhouse-r

# con <- dbConnect(clickhouse::clickhouse(), host="10.0.0.234", port=8123L, user="default", password="")
# реквизиты для подключения на удаленном стенде
con <- dbConnect(clickhouse(), host="172.16.33.74", port=8123L, user="default", password="")

# реквизиты для подключения на локальном стенде
# con <- dbConnect(clickhouse(), host="10.0.0.180", port=8123L, user="default", password="")

if (FALSE){
#dbWriteTable(con, "mtcars", mtcars)
# dbListTables(con)
# dbGetQuery(con, "SELECT COUNT(*) FROM mtcars")
# dbGetQuery(con, "SHOW TABLES")
# browser()
# mtrace(dbReadTable)
tt <- dbReadTable(con, "mtcars")
tt2 <- dbReadTable(con, "test")
tt3 <- dbReadTable(con, "events")
# rs <- dbSendQuery(con, "SELECT * FROM events")
# tt4 <- dbFetchetch(con)
# tt4 <- dbGetQuery(con, "SELECT COUNT(*) FROM mtcars")
tt4 <- dbGetQuery(con, "SELECT * FROM events")
tic()
tt5 <- dbGetQuery(con, "SELECT * FROM big_csv")  %>%
  mutate_if(is.character, `Encoding<-`, "UTF-8")
toc()
}

# получим данные через http интерфейс -------------------------------
tm <- "2017-05-04T13:55:00.302Z"
anytime(tm, tz="Europe/Moscow", asUTC=TRUE)
fastPOSIXct(tm, tz="Europe/Moscow") # на 5-10% медленнее

profvis({
  # посмотрим, какой ответ будет на функцию проверки изменений
  t <- dbGetQuery(con, "SELECT COUNT() FROM view_states")
  
  tic("Query")
  # rs <- dbSendQuery(con, 
  #                   "SELECT * FROM view_states WHERE toDate(begin) >= yesterday() AND begin < now()")
  rs <- dbSendQuery(con, 
                    "SELECT * FROM view_states WHERE begin >= toUInt32(yesterday()) AND begin < toUInt32(now())")
  
  toc()
  
  # на выходе ожидаем data.frame
  tic("Fetch")
  df0 <- dbFetch(rs) 
  toc()
  
  tic("Date processing")
  if (is.character(df0$begin)){
    df <- df0 %>% mutate_at(vars(begin, end), anytime, tz="Europe/Moscow", asUTC=FALSE)
    #df0$begin1 <- df0 %>% mutate_at(vars(begin), anytime, tz="Europe/Moscow", asUTC=FALSE)
  }
  toc()
  
})

# небольшой бенчмаркинг --------------------
packageVersion("microbenchmark")
#df$time <- as.numeric(df$begin)
#microbenchmark::microbenchmark(df %>% mutate_at(vars(time), anytime, tz="Europe/Moscow", asUTC=FALSE), unit="ms", times=2)
microbenchmark::microbenchmark(df0$begin1 <- anytime(df0$begin, tz="Europe/Moscow", asUTC=FALSE), unit="ms", times=2)
microbenchmark::microbenchmark(df1 <- mutate_at(df0, vars(begin), anytime, tz="Europe/Moscow", asUTC=FALSE), unit="ms", times=2)

# --------------------------------------------

gp <- ggplot(df, aes(x=duration)) +
  theme_bw() +
  geom_histogram(binwidth=1)

gp

# dbWriteTable(con, "mtcars")
# проверим маскирование кавычек
# dbq(con, "mtcars")

# dbWriteTable(con, "mtcars2", mtcars)
dbListTables(con)


# t <- dbGetQuery(con, "SELECT *  FROM mtcars")
# d <- dbReadTable(con, "mtcars")
dbDisconnect(con)
stop()
