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
