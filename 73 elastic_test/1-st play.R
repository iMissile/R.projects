library(tydyverse)
library(tibble)
library(purrr)
library(magrittr)
library(data.tree)
library(rlist)
library(reshape2)
library(pipeR)
library(elastic)


connect(es_host="89.223.29.108", es_port = 9200)
info() # посмотрели информацию про соединение

t1 <- cat_indices(parse=TRUE) # получаем список индексов
t2 <- cat_indices(verbose=TRUE, "?format=json&pretty") # так работает, но JSON выдает на экран
t3 <- capture.output(cat_indices(verbose=FALSE, "?format=json&pretty")) # об€зательно должен быть первый параметр
t4 <- cat_indices(parse=TRUE, "?format=json&pretty") # так не рабоает, нальз€ распарсить в data.frame

# смотрим индексы в браузере
# http://89.223.29.108:9200/_cat/indices?v

m <- Search(index="packetbeat-*", size=100)$hits$hits


# ------------ делаем выборку переданных байт по source.ip за заданный промежуток времени

# https://www.elastic.co/guide/en/elasticsearch/reference/5.3/common-options.html#date-math
body <- list(query=list(range=list(start_time=list(gte="now-1d/d", lte="now"))))
Search(index="packetbeat-*", body=body)$hits$total
m <- Search(index="packetbeat-*", body=body)$hits$hits # получаем только 10 элементов вместо Total=129k (default size=10)

# ------------- пишем хитрый запрос через отдельный json

# так выдаетс€ очень много результатов (>10K, надо делать scroll)
body <- '{
  "query": {
    "bool": {
      "must": [ 
        {"match": { "source.ip": "10.0.0.232"}} 
      ],
      "filter":  [
        { "range": { "start_time": { "gte": "now-1d", "lte": "now" }}}
      ] 
    }
  }
}'
Search(index="packetbeat-*", body=body, source="source.stats.net_bytes_total")$hits$total
# получим только интересующие пол€, пробелов между пол€ми быть не должно
m <- Search(index="packetbeat-*", body=body, size=10000, sort="source.stats.net_bytes_total:desc",
            source="start_time,source.port,source.ip,source.stats.net_bytes_total")$hits$hits

# ------------- делаем scroll, чтобы вытащить все элементы
body <- '{
  "query": {
    "bool": {
      "must": [ 
        {"match": { "source.ip": "10.0.0.232"}} 
      ],
      "filter":  [
        { "range": { "start_time": { "gte": "now-1d", "lte": "now" }}}
      ] 
    }
  }
}'

# надо оптимизировать размер size исход€ из совокупного времени исполнени€ запросов + склейки
req_size <- 2
res <- Search(index="packetbeat-*", body=body, scroll="1m", size=req_size)
m <- res$hits$hits

jl <- as.Node(m)
list.names(m)
rl <- list.select(m, type=`_type`, timestamp=`_source`$`@timestamp`) # так достаетс€ с помощью rlist
rl <- list.select(m, start_time=`_source`$`start_time`, 
                  src_ip=`_source`$source$ip,
                  src_port=`_source`$source$port,
                  bytes_sent=`_source`$source$stats$net_bytes_total) # так достаетс€ с помощью rlist

# df <- list.stack(rl) # выдает ошибку, если поле NULL: Error in data.table::rbindlist(.data, ...) : Column 3 of item 1 is length 0, inconsistent 

rl2 <- map(rl, function(x){str(x); cat("--"); map(x, function(z) ifelse(is.null(z), NA, z))})

dplyr::bind_rows(rl) # когда нет полей NULL, то все срабатывает
dplyr::bind_rows(rl2) # когда нет полей NULL, то все срабатывает
# melt(rl) # можно и так + gather

map_val_list <- list(c("start_time", "start_time"),
                     c("src_port", "source$port"))

# см. фукнцию get_tsdf_from_SW_json
df <- map(m, 
          function(x){
            str(x)
            tibble(start_time=x$`_source`$start_time,
                   src_port=x$`_source`$source$port)
          }
          )

# надо пробежатьс€ по всему списку, достать нужные параметры и замапировать их на пол€ data.frame) 

# взгл€нем на длину данных и длину скрола
res$hits$total
length(scroll(scroll_id = res$`_scroll_id`)$hits$hits)
req_list <- seq(from=1, by=1, length.out=res$hits$total/req_size)


processScroll <- function(n){
  cat("iteration", n, "\n")
}
  
# склеиваем все запросы
df <- req_list %>%
  purrr::map_df(processScroll, .id = NULL)

# ========
Search(index="packetbeat-*", body=body)$hits$total
m <- Search(index="packetbeat-*", body=body)$hits$hits # получаем только 10 элементов вместо Total=129k (default size=10)



body <-
  list(query = list(range = list(start_time = list(
    gte = "now-1d/d", 
    lte = "now"
  ))))
