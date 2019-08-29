# library(MonetDBLite)
# https://github.com/MonetDB/MonetDBLite-R

library(DBI)
library(data.table)
library(tictoc)
library(bigreadr)


con <- dbConnect(MonetDBLite::MonetDBLite(), here::here("db"))

# читаем большой фалик от FP:

fname <- "D:/iwork.GL/fix-price/big_data/sales.csv" # 200 млн строк
format(file.size(fname), big.mark = ",")
nlines(fname)


tic("Load Sales data")
getDTthreads()
# читаем не все, памяти мало
# dt <- fread(file = , nrows = 0, showProgress = TRUE, verbose = TRUE)
big_dt <- big_fread1(file = fname, every_nlines = 5e5, showProgress = TRUE, verbose = TRUE)

toc()

# Shutdown
dbDisconnect(con, shutdown=TRUE)
MonetDBLite::monetdblite_shutdown()