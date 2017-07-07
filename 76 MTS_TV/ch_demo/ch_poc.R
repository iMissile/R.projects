# ODBC DSN: Name=ClickHouse, Host=10.0.0.234, Port=8123, Database=default, user=default
library(RODBC)
# library(RODBCDBI)
con <- dbConnect(RODBCDBI::ODBC(), dsn='CH_unicode', believeNRows=FALSE, rows_at_time=1)
rs <- dbSendQuery(con, 'select 1')
dbFetch(rs)

rs <- dbSendQuery(con, "SELECT * FROM events")
dbFetch(rs)

dbClearResult(rs)
dbDisconnect(con)


stop()
