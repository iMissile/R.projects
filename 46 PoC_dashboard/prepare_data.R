# генерируем тестовые файлы для отладки интерфейса
#library(tidyr)
library(ggplot2)

generate_field_data <- function() {
  # данные по расположению сенсоров
  sensor <- data.frame(
    c(1, 37.578691607470724, 55.766160765720493),
    c(2, 37.57990362015564, 55.766504551149772),
    c(3, 37.580025839922193, 55.765874275547077),
    c(4, 37.577632369493983, 55.764819973580607),
    c(5, 37.581431367237478, 55.764384492709446),
    c(6, 37.579109191673091, 55.765158040903287),
    c(7, 37.58182858147876, 55.766160765720493),
    c(8, 37.580586013852198, 55.767008764295831),
    c(9, 37.579862880233463, 55.764109449653162),
    c(10, 37.577856439065989, 55.766567578149633),
    c(11, 37.576776831128157, 55.76609773806296)
  )
  # sensor <- t(sensor) #транспонируем
  # sensor <- tFrame(sensor) #транспонируем
  sensor <- as.data.frame(t(sensor))
  rownames(sensor) <- NULL
  colnames(sensor) <- c("name", "lon", "lat") # http://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame-in-r
  
  #sensor <- as.data.frame(t(sensor)) #транспонируем
  # dimnames(sensor) <- NULL
  
  # генерируем данные по показаниям сенсоров (почасовая раскладка)
  tick.seq <- seq(as.POSIXct("2016-03-01 23:00:00"), as.POSIXct("2016-05-02 08:32:00"), by = "hour") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  # собираем сразу data.frame: время, #сенсора, показание
  mydata <- data.frame(name = rep(sensor$name, each = length(tick.seq)), timestamp = tick.seq, temp = rnorm(nrow(sensor)*length(tick.seq), 15, 5)) # используем методику дополнения
  # rnorm(1000, 3, .25) # Generates 1000 numbers from a normal with mean 3 and sd=.25
  
  qplot(timestamp, temp, data = mydata)
  
  write.table(
    mydata,
    file = ".\\data\\tsensors.csv",
    sep = ",",
    row.names = FALSE,
    qmethod = "double"
  )
}


# =================== main ==================

generate_field_data()