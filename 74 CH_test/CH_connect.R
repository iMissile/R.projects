library(DBI)
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
con <- dbConnect(clickhouse(), host="10.0.0.234", port=8123L, user="default", password="")
#dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) FROM mtcars")
dbGetQuery(con, "SHOW TABLES")
browser()
# mtrace(dbReadTable)
d <- dbReadTable(con, "mtcars")
dbDisconnect(con)
stop()

con <- dbConnect(odbc::odbc(),
                 driver = "ClickHouse ANSI",
                 #driver = "ClickHouse Unicode",
                 DATABASE = "default",
                 UID = "default",
                 PWD = "",
                 #HOST = "10.0.0.234",
                 host = "10.0.0.234",
                 PORT=8123L
                 #port=8123L
                 )

dput(con)

#dbWriteTable(con, "mtcars", mtcars)
#dbListTables(con)
stop()

if(FALSE){
  # сохраним тестовые данные
  cars_df <- mtcars %>% tibble::rownames_to_column()
  write_csv(cars_df, "mtcars.csv")
}

library(RODBC)
# Make a connection using your DSN name
conn <- odbcConnect("CH_demo")

sqlTables(conn)
sqlSave(conn, mtcars)
sqlTables(conn)

close(conn)

# 
# con <- dbConnect(odbc::odbc(),
#                  Driver    = "ClickHouse ANSI", 
#                  Server    = "10.0.0.234",
#                  Database  = "default",
#                  #UID       = [My User ID],
#                  #PWD       = [My Password],
#                  Port      = 8123)
# host="localhost", port=8123L, user="default", password=""