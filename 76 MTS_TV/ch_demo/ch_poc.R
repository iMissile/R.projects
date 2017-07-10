# ODBC DSN: Name=ClickHouse, Host=10.0.0.234, Port=8123, Database=default, user=default
library(DBI)
library(RODBC)
# library(RODBCDBI)
con <- dbConnect(RODBCDBI::ODBC(), dsn='CH_ANSI', believeNRows=FALSE, rows_at_time=1)

if(FALSE){
rs <- dbSendQuery(con, 'select 1')
dbFetch(rs)

rs <- dbSendQuery(con, "SELECT * FROM test")
dbFetch(rs)

rs <- dbSendQuery(con, "SELECT * FROM mtcars")
m <- dbFetch(rs)

rs <- dbSendQuery(con, "SELECT * FROM events")
t <- dbFetch(rs)

rs <- dbSendQuery(con, "SELECT * FROM states")
t <- dbFetch(rs)
}

tic()
# посмотрим, какой ответ будет на функцию проверки изменений
rs <- dbSendQuery(con, "SELECT COUNT() FROM states")
t <- dbFetch(rs)
rs <- dbSendQuery(con, "SELECT * FROM states WHERE toDate(begin) >= yesterday() AND begin < now()")
# на выходе ожидаем data.frame
df <- dbFetch(rs)
toc()

dbClearResult(rs)
dbDisconnect(con)

gp <- ggplot(df, aes(x=duration)) +
  theme_bw() +
  geom_histogram(binwidth=1)

gp



stop()
