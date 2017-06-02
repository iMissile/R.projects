library(DBI)

# https://rviews.rstudio.com/2017/05/17/databases-using-r/
# https://github.com/yandex/clickhouse-odbc
# https://github.com/rstats-db/odbc

con <- dbConnect(odbc::odbc(),
                 driver = "ClickHouse ANSI",
                 database = "default",
                 UID = "default",
                 PWD = "",
                 SERVER = "10.0.0.234",
                 PORT=8123L
                 )
# 
# con <- dbConnect(odbc::odbc(),
#                  Driver    = "ClickHouse ANSI", 
#                  Server    = "10.0.0.234",
#                  Database  = "default",
#                  #UID       = [My User ID],
#                  #PWD       = [My Password],
#                  Port      = 8123)
# host="localhost", port=8123L, user="default", password=""