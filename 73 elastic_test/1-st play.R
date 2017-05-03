library("elastic")


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


body <- list(query=list(bool=list(must=list(match_all=''), filter=(match=list(source.ip="10.0.0.232")))))

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
m <- Search(index="packetbeat-*", body=body, size=100, sort="source.stats.net_bytes_total:desc",
            source="start_time,source.port,source.ip,source.stats.net_bytes_total")$hits$hits

# ========
Search(index="packetbeat-*", body=body)$hits$total
m <- Search(index="packetbeat-*", body=body)$hits$hits # получаем только 10 элементов вместо Total=129k (default size=10)



body <-
  list(query = list(range = list(start_time = list(
    gte = "now-1d/d", 
    lte = "now"
  ))))
