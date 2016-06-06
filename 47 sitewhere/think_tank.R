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
library(ggdendro) # дл€ пустой темы
#library(ggmap)
library(RColorBrewer) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
library(scales)
library(gtable)
library(grid) # дл€ grid.newpage()
library(gridExtra) # дл€ grid.arrange()



req <- try({
  curl_fetch_memory("1https://raw.githubusercontent.com/iot-rus/Moscow-Lab/master/weather.txt")
  # status_code == 200
  # class(try-error)
})
# проверим только 1-ый элемент класса, поскльку при разных ответах получаетс€ разное кол-во элементов
if(class(req)[[1]] != "try-error" && req$status_code == 200) {
  # ответ есть, и он корректен. ¬ этом случае осуществл€ем пребразование 
  print("conversion")
} else {
  print("error")
}
  