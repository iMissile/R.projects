library(tidyverse)
library(DBI)
library(tictoc)
# library(debug)
# library(readr)
# library(tidyverse)
# library(clickhouse)
source("clickhouse.R")


# https://rviews.rstudio.com/2017/05/17/databases-using-r/
# https://github.com/yandex/clickhouse-odbc
# https://github.com/rstats-db/odbc


# https://github.com/hannesmuehleisen/clickhouse-r

# con <- dbConnect(clickhouse::clickhouse(), host="10.0.0.234", port=8123L, user="default", password="")
con <- dbConnect(clickhouse(), host="10.0.0.180", port=8123L, user="default", password="")
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


# посмотрим, какой ответ будет на функцию проверки изменений
t <- dbGetQuery(con, "SELECT COUNT() FROM states")

# dbWriteTable(con, "mtcars")
# проверим маскирование кавычек
# dbq(con, "mtcars")

# dbWriteTable(con, "mtcars2", mtcars)
dbListTables(con)


t <- dbGetQuery(con, "SELECT *  FROM mtcars")

d <- dbReadTable(con, "mtcars")
dbDisconnect(con)
stop()
