# library(MonetDBLite)
# https://github.com/MonetDB/MonetDBLite-R

library(DBI)


con <- dbConnect(MonetDBLite::MonetDBLite(), here::here("db"))

# Shutdown
dbDisconnect(con, shutdown=TRUE)
MonetDBLite::monetdblite_shutdown()