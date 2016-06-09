# разбираемся с обработкой данных с sitewhere
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(magrittr)
library(curl)
library(httr)
library(ggthemes)
library(ggdendro) # для пустой темы
#library(ggmap)
library(RColorBrewer) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
library(scales)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()


user_name = 'admin'
user_pass = 'password'
base_url = paste0('http://', user_name, ':', user_pass, '@10.0.0.207:28081/sitewhere/api/')
t_token = 'sitewhere1234567890'


# http://10.0.0.207:28081/sitewhere/api/mt/assets/modules/fs-locations/assets/harry-dirt-pot/assignments/?siteToken=c08a662e-9bbb-4193-a17f-96e0c760e1c3&tenantAuthToken=sitewhere1234567890
# магическая строка для получения измерений по assignment

url <- paste0(base_url, "mt/assets/modules/fs-locations/assets/harry-dirt-pot/assignments/?siteToken=c08a662e-9bbb-4193-a17f-96e0c760e1c3&tenantAuthToken=", 
              t_token)
req <- curl_fetch_memory(url)
write(rawToChar(req$content), file="./temp/resp.txt")
data <- fromJSON(rawToChar(req$content))
# ищем assignments с sensor_type = soil_moisture
stop()

d <- toJSON(list(var1 = 34, var2 = c('rr', 'mm')), pretty = TRUE, auto_unbox = TRUE)

url <- paste0(base_url, "mt/assets/categories/fs-locations/assets/harry-dirt-pot/property/soil_moisture_ts?tenantAuthToken=", 
              t_token)
resp <- curl_fetch_memory(url)


stop()

req <- curl_fetch_memory("http://admin:password@10.0.0.207:28081/sitewhere/api/assets/categories/fs-locations/assets?tenantAuthToken=sitewhere1234567890")
# wrecs <- rawToChar(req$content)
data <- fromJSON(rawToChar(req$content))


# про POST() см. тут: https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html


d <- toJSON(list(var1 = 34, var2 = c('rr', 'mm')), pretty = TRUE, auto_unbox = TRUE)
url <- "http://admin:password@10.0.0.207:28081/sitewhere/api/assets/categories/fs-locations/locations/harry-dirt-pot?tenantAuthToken=sitewhere1234567890"
body <- list(id = "harry-dirt-pot", properties = list(worksite.id = d))

r <- PUT(url, body = body, encode = "json")
stop()

url <- "api.openweathermap.org/data/2.5/"   
MoscowID <- '524901'
APPID <- '19deaa2837b6ae0e41e4a140329a1809'
resp <- GET(paste0(url, "weather?id=", MoscowID, "&APPID=", APPID))
