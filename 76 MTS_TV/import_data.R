library(tidyverse)
library(tibble)
library(purrr)
library(magrittr)
#library(data.tree)
#library(rlist)
#library(reshape2)
#library(pipeR)
library(jsonlite)
library(tidyjson)
#library(elastic)
#library(parsedate)
#library(fasttime)
#library(anytime)
library(microbenchmark)
library(tictoc)

# https://jsonformatter.curiousconcept.com/

fname <- "./data/head.json"

tic()
# заменим концы строк на `,`` и добавим шапочку и окончание для формирования семантически правильного json
# последнюю ',' надо удалить, может такое встретиться (перевод строки)
# tmp <- paste0('{"res":[', gsub("\\n", ",\n", wrecs, perl = TRUE), ']}')

df <- jsonlite::fromJSON(fname, simplifyDataFrame=TRUE)
toc()
