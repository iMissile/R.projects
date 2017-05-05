library(tidyverse)
library(lubridate)
library(parsedate)
library(fasttime)
library(anytime)
# library(tibble)
library(purrr)
library(magrittr)
# library(data.tree)
# library(rlist)
# library(reshape2)
# library(pipeR)
# library(jsonlite)
library(tidyjson)
library(profvis)
library(elastic)
library(microbenchmark)

source("funcs.R")

# main -----------------------------

connect(es_host="89.223.29.108", es_port = 9200)
info() # посмотрели информацию про соединение

# --------------------------------------------------------------------------
# делаем scroll, чтобы вытащить все элементы, делаем через JSON ------------
body <- '{
"query": {
"bool": {
"must": [ 
{"match": { "source.ip": "10.0.0.232"}} 
],
"filter":  [
{ "range": { "start_time": { "gte": "now-8h", "lte": "now" }}}
] 
}
}
}'

# надо оптимизировать размер size исходя из совокупного времени исполнения запросов + склейки
req_size <- 200 # так получается наиболее оптимально по времени исполнению\задействованной памяти

#profvis({
res <- Search(index="packetbeat-*", body=body, scroll="1m", size=req_size)

# взглянем на длину данных и длину скрола

req_list <- seq(from=1, by=1, length.out=res$hits$total/req_size)

cat(res$hits$total, ",  ", req_list, "\n")

# надо пробежаться по всему списку, достать нужные параметры и замапировать их на поля data.frame
df <- req_list %>%
  purrr::map_df(processScroll, scroll_id=res$`_scroll_id`, req_size=req_size, .id = NULL)
#})

# надо проверить, что df не пустой (0 объектов)
# browser()
# преобразуем типы
df2 <- df %>%
  mutate(timestamp=fastPOSIXct(start_time)) %>% # время указано в ISO 8601
  mutate(timegroup=hgroup.enum(timestamp, min_bin=5))
