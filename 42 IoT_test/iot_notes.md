
# Платформа IoT
- [EasyIoT Cloud REST API V1.0](http://iot-playground.com/blog/2-uncategorised/78-easyiot-cloud-rest-api-v1-0)
- Credentials
	* username: pkochnov
	* pass: Qwerty123
	* instanceId: 56d57092c943a05b64ba2682
- Периодическое получение последних данных (видимо, это для обновления цифирек и показаний индикаторов)
```
http://cloud.iot-playground.com/Api/Realtime/EventStream?authorization=cGtvY2hub3Y6UXdlcnR5MTIz&username=pkochnov&lastEventId=145680135300000
```

- Авторизацию под вышеуказанные Credentials берем из трассировки HTM запроса со страницы [Control Chart](http://cloud.iot-playground.com/#page_control_chart): `Authorization:Basic ZWFzeWlvdDplYXN5aW90`
- Запрос графика:
```
GET /query?q=select+time%2C+Value+from+hist+where+Id+%3D+%27Dg3i5sJyPsXse53O%27+and+time+%3E+now()+-+1w&db=eiotclouddb&epoch=ms&_=1456921063016
```
Текущее время указывается в миллисекундах, онлайн преобразователь можно использовать [здесь](http://currentmillis.com/)
В декодированном виде (средства разработчика Chrome или [онлайн преобразователь](http://www.url-encode-decode.com/)) это выглядит так:
```
GET /query?q=select time, Value from hist where Id = 'Dg3i5sJyPsXse53O' and time > now() - 1w&db=eiotclouddb&epoch=ms&_=1456921063016
```
Этот Id числится за `Sensor.BandwidthIngress`

# Web
- [Common URL Escape Characters](http://www.werockyourweb.com/url-escape-characters/)
- [HTML URL Encoding Reference](http://www.w3schools.com/tags/ref_urlencode.asp)
- [URL Encode/Decode](http://www.url-encode-decode.com/)

# Python ссылки
- [InfluxDB-Python installation](http://influxdb-python.readthedocs.org/en/latest/include-readme.html#id2)
- [Requests](http://docs.python-requests.org/en/latest/user/quickstart/)
    
# R ссылки
- [Web scraping in R with jsonlite](http://stackoverflow.com/questions/31081016/web-scraping-in-r-with-jsonlite)
```R
library("httr"); 
library("plyr"); 
library("jsonlite")
getData <- function(start, end) {
  base_url <- "https://api.github.com/users/"
  ldply(start:end, function(num) {
    cat(url <- paste0(base_url,num, "/repos"), "\n")
    resp <- GET(url)
    if (status_code(resp) == 200) {
      df <- fromJSON(content(resp, "text"))
      out <- data.frame(language = NA, name = NA)
      if (length(df) > 0) {
        out <- df[, c("language", "name")]
      }
    }
    out
  })
}
```


# отображение charts в cloud.iot-playground

как выглядит запрос
q=select+time%2C+Value+from+hist+where+Id+%3D+%27hBuOYDpUnewPKMG6%27+and+time+%3E+now()+-+1d&db=eiotclouddb&epoch=ms&_=1456906918710

# запрос данных
```
curl "http://cloud.iot-playground.com/Api/Realtime/EventStream?authorization=cGtvY2hub3Y6UXdlcnR5MTIz&username=pkochnov&lastEventId=145690754796540&r=2130055155139416" -H "Accept-Encoding: gzip, deflate, sdch" -H "Accept-Language: ru,en-US;q=0.8,en;q=0.6" -H "User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.116 Safari/537.36" -H "Accept: text/event-stream" -H "Referer: http://cloud.iot-playground.com/" -H "Cookie: _ga=GA1.2.1134358086.1456832840; sel_group=groupdiv_0" -H "Connection: keep-alive" --compressed

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
```
    
# Тестовые русские символы для проверки кодировки
