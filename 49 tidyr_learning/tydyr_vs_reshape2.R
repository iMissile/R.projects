# http://www.milanor.net/blog/reshape-data-r-tidyr-vs-reshape2/


library(dplyr)
library(tidyr)
# From http://stackoverflow.com/questions/1181060
stocks <- data_frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

stocksm <- gather(stocks, stock, price, -time)
spread.stock <- spread(stocksm, stock, price)
head(spread.stock)
