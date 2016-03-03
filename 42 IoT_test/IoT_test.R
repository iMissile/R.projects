library(influxdbr)
library(curl)
library(jsonlite)
library(dplyr)
library(magrittr)
library(httr)
# library(TriMatch) # as.data.frame.list {TriMatch}

# Fetching JSON data from REST APIs  (https://cran.r-project.org/web/packages/jsonlite/vignettes/json-apis.html)
# URL: http://cloud.iot-playground.com:40404/RestApi/v1.0/Parameter/[id]/Value

# username - pkochnov
# pass Qwerty123
# instanceId = 56d57092c943a05b64ba2682

# Get Token
url <- "http://cloud.iot-playground.com:40404/RestApi/v1.0/Token/List"
instanceID <- "56d57092c943a05b64ba2682"
resp <- GET(url, add_headers("Eiot-Instance" = instanceID))
tokenID <- content(resp)[[1]]$Token  # Advanced R, 3.2 Subsetting operators

# Get Module List
url <- "http://cloud.iot-playground.com:40404/RestApi/v1.0/Module/List"
resp <- GET(url, add_headers("EIOT-AuthToken" = tokenID))
cont <- content(resp)

# Get Parameter List
url <- "http://cloud.iot-playground.com:40404/RestApi/v1.0/Parameter/List"
resp <- GET(url, add_headers("EIOT-AuthToken" = tokenID))
cont <- content(resp)

# снимаем исторические данные грязным хаком
url <- "cloud.iot-playground.com:8086"
query <- "/query?q=select+time%2C+Value+from+hist+where+Id+%3D+%27Dg3i5sJyPsXse53O%27+and+time+%3E+now()+-+1d&db=eiotclouddb&epoch=ms&_=1456921063016"
resp <- GET(paste0(url, query), add_headers("Authorization" = "Basic ZWFzeWlvdDplYXN5aW90"))
cont <- content(resp)

# cont <- fromJSON('{"results":[{"series":[{"name":"hist","columns":["time","Value"],"values":[[1456842993897,1000],[1456843135556,1000],[1456843148417,1000],[1456843536525,2.667e+06],[1456843542450,2.67e+06],[1456843548310,2.672e+06],[1456843554039,2.674e+06]]}]}]}')

# достаем список значений {time, Value}
measure <- cont$results[[1]]$series[[1]]$values
# получили список векторов
t <- lapply(measure, function(x) c(x[[1]], x[[2]]))
t <- lapply(measure, function(x) data.frame(timestamp = x[[1]], value = x[[2]]))




lapply(measure, function(x) print(paste0(x[1], "---", x[2])))

measure_df <- data.frame(timestamp = numeric(0), value = integer(0))

lapply(measure, function(x) dplyr::bind_rows(measure_df, data.frame(x[1], x[2])))


# !!! работает !!!
# http://stackoverflow.com/questions/4227223/r-list-to-data-frame
# t2 <- data.frame(Reduce(rbind, t))
t2 <- as.data.frame.list(t, row.names = c("timestamp", "value"))


# =====================================
# пробуем по другому, для нормальной работы надо преобразовать данные в data.frame
# https://jeroenooms.github.io/mongo-slides/#11
# cc <- curl("https://api.github.com/repos/hadley/ggplot2/issues") %>%
#   fromJSON(flatten = TRUE) %>%
#   mutate(date = as.Date(created_at)) %>%
#   filter(user.login == "hadley") %>%
#   select(title, state, date)

cc <- GET("https://api.github.com/repos/hadley/ggplot2/issues")
# curl(paste0(url, query), add_headers("Authorization" = "Basic ZWFzeWlvdDplYXN5aW90")) %>%
t <- fromJSON(content(cc, "text"), flatten = TRUE)
  
stop()

# influxdbr
# https://github.com/dleutnant/influxdbr
# http://influxdb-python.readthedocs.org/en/latest/examples.html

# create influx connection object
con <- influxdbr::influx_connection(host = "cloud.iot-playground.com",
                                    port = 8086,
                                    user = "pkochnov",
                                    pass = "Qwerty123")

#influxdbr::show_databases(con = con) # под рутом надо делать...

# show measurements
influxdbr::show_measurements(con = con, db = "eiotclouddb")

# reverse запрос данных
url <- "http://cloud.iot-playground.com:8086"
query <- '/query?q=select+time"%"2C+Value+from+hist+where+Id+"%"3D+"%"27hBuOYDpUnewPKMG6"%"27+and+time+"%"3E+now()+-+1d&db=eiotclouddb&epoch=ms&_=1456906918710'

req <- getURI(paste0(url, query))

stop()

# запрос данных
curl "http://cloud.iot-playground.com:8086/query?q=select+time"%"2C+Value+from+hist+where+Id+"%"3D+"%"27hBuOYDpUnewPKMG6"%"27+and+time+"%"3E+now()+-+1d&db=eiotclouddb&epoch=ms&_=1456906918710" -X OPTIONS -H "Access-Control-Request-Method: GET" -H "Origin: http://cloud.iot-playground.com" -H "Accept-Encoding: gzip, deflate, sdch" -H "Accept-Language: ru,en-US;q=0.8,en;q=0.6" -H "User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36" -H "Accept: */*" -H "Referer: http://cloud.iot-playground.com/" -H "Connection: keep-alive" -H "Access-Control-Request-Headers: accept, authorization" --compressed
OPTIONS /query?q=select+time%2C+Value+from+hist+where+Id+%3D+%27hBuOYDpUnewPKMG6%27+and+time+%3E+now()+-+1d&db=eiotclouddb&epoch=ms&_=1456906918710 HTTP/1.1
Host: cloud.iot-playground.com:8086
Connection: keep-alive
Access-Control-Request-Method: GET
Origin: http://cloud.iot-playground.com
User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36
Access-Control-Request-Headers: accept, authorization
Accept: */*
  Referer: http://cloud.iot-playground.com/
  Accept-Encoding: gzip, deflate, sdch
Accept-Language: ru,en-US;q=0.8,en;q=0.6

HTTP/1.1 200 OK
Access-Control-Allow-Headers: Accept, Accept-Encoding, Authorization, Content-Length, Content-Type, X-CSRF-Token, X-HTTP-Method-Override
Access-Control-Allow-Methods: DELETE, GET, OPTIONS, POST, PUT
Access-Control-Allow-Origin: http://cloud.iot-playground.com
Content-Encoding: gzip
Content-Type: application/json
Request-Id: 035177e1-e050-11e5-b777-000000000000
X-Influxdb-Version: 0.9.4.2
Date: Wed, 02 Mar 2016 08:23:16 GMT
Content-Length: 184




# RCurl helper
# get auth token
url <- "http://cloud.iot-playground.com:40404/RestApi/v1.0/Token/List"
instanceID <- "56d57092c943a05b64ba2682"

req <- getURI(url, httpheader = paste0("Eiot-Instance:", instanceID))

 
stop()

library(plotly)

Sys.setenv("plotly_username"="iMissile")
Sys.setenv("plotly_api_key"="g6c59jc0jq")


p <- plot_ly(midwest, x = percollege, color = state, type = "box")

p

# plotly_POST(p, filename = "r-docs/midwest-boxplots", world_readable=TRUE)
plotly_POST(p, filename = "r-docs/midwest-boxplots", fileopt = "overwrite", sharing="public")

plotly_POST(p, filename='privacy-secret', sharing='hidden')
