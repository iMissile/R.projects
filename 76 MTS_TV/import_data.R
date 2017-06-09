library(tidyverse)
library(tibble)
library(purrr)
library(magrittr)
#library(data.tree)
#library(rlist)
#library(reshape2)
#library(pipeR)
library(jsonlite)
#library(tidyjson)
#library(elastic)
#library(parsedate)
#library(fasttime)
#library(anytime)
library(microbenchmark)
library(tictoc)

# https://jsonformatter.curiousconcept.com/

if (FALSE){
fname <- "./data/head.json"
fname <- "./data/stat_fix.json"
# fname <- "./data/stat_small.json"


tic()
# заменим концы строк на `,`` и добавим шапочку и окончание для формирования семантически правильного json
# последнюю ',' надо удалить, может такое встретиться (перевод строки)
# tmp <- paste0('{"res":[', gsub("\\n", ",\n", wrecs, perl = TRUE), ']}')
tvstream <- stream_in(file("./data/stat_small.json"), pagesize = 10000)

#df <- jsonlite::fromJSON(fname, simplifyDataFrame=TRUE)
toc()

system.time(saveRDS(tvstream, "tvstream2.rds", ascii=FALSE, compress="gzip"))
}

system.time(df <- readRDS("./data/tvstream2.rds"))