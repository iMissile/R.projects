library(tidyverse)
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

# functions -----------------------
processScroll <- function(n, scroll_id, req_size){
  cat("iteration", n, "\n")
  
  scroll_json <- scroll(scroll_id=scroll_id, raw=TRUE, size=req_size)
  
  # http://stackoverflow.com/questions/35198991/tidyjson-is-there-an-exit-object-equivalent
  # делаем в два шага
  df <-
    scroll_json %>% enter_object("hits") %>% enter_object("hits") %>%
    gather_array %>% enter_object("_source") %>%
    spread_values(
      start_time=jstring("start_time")
    ) %>%
    enter_object("source") %>%
    spread_values(
      src_ip=jstring("ip"),
      src_port=jnumber("port")
    ) %>%
    enter_object("stats") %>%
    spread_values(
      bytes_total=jnumber("net_bytes_total")
    ) %>%
    select(-document.id)
  
  # then enter an object and get something from inside, merging it as a new column
  df <- merge(df, 
              scroll_json %>% enter_object("hits") %>% enter_object("hits") %>%
                gather_array %>% 
                enter_object("_source") %>%
                enter_object("dest") %>%
                spread_values(
                  dst_ip=jstring("ip"),
                  dst_port=jnumber("port")
                ) %>% select(-document.id),
              by = c('array.index'))
  
  df
}


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
{ "range": { "start_time": { "gte": "now-4h", "lte": "now" }}}
] 
}
}
}'

# надо оптимизировать размер size исходя из совокупного времени исполнения запросов + склейки
req_size <- 200 # так получается наиболее оптимально по времени исполнению\задействованной памяти

profvis({
res <- Search(index="packetbeat-*", body=body, scroll="1m", size=req_size)

# взглянем на длину данных и длину скрола

req_list <- seq(from=1, by=1, length.out=res$hits$total/req_size)

cat(res$hits$total, ",  ", req_list, "\n")

# надо пробежаться по всему списку, достать нужные параметры и замапировать их на поля data.frame
df <- req_list %>%
  purrr::map_df(processScroll, scroll_id=res$`_scroll_id`, req_size=req_size, .id = NULL)
})
