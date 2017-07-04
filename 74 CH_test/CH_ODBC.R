# ODBC DSN: Name=ClickHouse, Host=10.0.0.234, Port=8123, Database=default, user=default
library(RODBC)
library(RODBCDBI)
con <- dbConnect(RODBCDBI::ODBC(), dsn='CH_unicode', believeNRows=FALSE, rows_at_time=1)
res <- dbSendQuery(con, 'select 1')
dbFetch(res)

stop()

# ODBC DSN: Name=ClickHouse, Host=10.0.0.234, Port=8123, Database=default, user=default
# https://github.com/rstats-db/odbc
# Compared to the existing RODBC package, odbc is faster (~3x for reading, ~2x for writing)
library(DBI)
# con <- dbConnect(odbc::odbc(), dsn='CH_unicode', believeNRows = FALSE, rows_at_time = 1)
con <- dbConnect(odbc::odbc(), dsn="CH_unicode", believeNRows=FALSE, rows_at_time=1)
res <- dbSendQuery(con, 'select 1')
dbFetch(res)

stop()


# ======================================
library(DBI)
library(RODBC)
# Make a connection using your DSN name
conn <- odbcConnect("CH_demo")

sqlTables(conn)
sqlSave(conn, mtcars)
sqlTables(conn)


con <- dbConnect(odbc::odbc(),
                 driver = "ClickHouse ANSI",
                 #driver = "ClickHouse Unicode",
                 DATABASE = "default",
                 UID = "default",
                 PWD = "",
                 HOST = "10.0.0.234",
                 #host = "10.0.0.234",
                 #PORT=8123L
                 port=8123L
)

dput(con)

  #dbWriteTable(con, "mtcars", mtcars)
dbListTables(con)

close(conn)
