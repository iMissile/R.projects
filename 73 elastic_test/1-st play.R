library("elastic")


connect(es_host="89.223.29.108", es_port = 9200)
info() # посмотрели информацию про соединение

t1 <- cat_indices(parse=TRUE) # получаем список индексов
t2 <- cat_indices(verbose=TRUE, "?format=json&pretty") # так работает, но JSON выдает на экран
t3 <- capture.output(cat_indices(verbose=FALSE, "?format=json&pretty")) # обязательно должен быть первый параметр
t4 <- cat_indices(parse=TRUE, "?format=json&pretty") # так не рабоает, нальзя распарсить в data.frame

# смотрим индексы в браузере
# http://89.223.29.108:9200/_cat/indices?v

m <- Search(index="packetbeat-*", size=100)$hits$hits
