rm(list=ls()) # î÷èñòèì âñå ïåðåìåííûå

library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(readr)
library(jsonlite)
library(magrittr)
library(futile.logger)


generate_field_data <- function(back_days = 7, forward_days = 7) {
  # äàííûå ïî ðàñïîëîæåíèþ ñåíñîðîâ
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
  # sensor <- t(sensor) #òðàíñïîíèðóåì
  # sensor <- tFrame(sensor) #òðàíñïîíèðóåì
  sensor <- as.data.frame(t(sensor))
  rownames(sensor) <- NULL
  colnames(sensor) <- c("name", "lon", "lat") # http://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame-in-r
  
  #sensor <- as.data.frame(t(sensor)) #òðàíñïîíèðóåì
  # dimnames(sensor) <- NULL
  
  # ãåíåðèðóåì äàííûå ïî ïîêàçàíèÿì ñåíñîðîâ (ïî÷àñîâàÿ ðàñêëàäêà)
  # tick.seq <- seq(as.POSIXct("2016-03-01 23:00:00"), as.POSIXct("2016-03-10 08:32:00"), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  start_time <- round_date(now(), unit = "day") # äëÿ ñèíõðîííûõ 4-õ ÷àñîâûõ èçìåðåíèé íåîáõîäèìî ïðèâåñòè ê 0:00
  tick.seq <- seq(start_time - days(back_days), start_time + days(forward_days), by = "4 hours") # http://stackoverflow.com/questions/10887923/hourly-date-sequence-in-r
  # ñîáèðàåì ñðàçó data.frame: âðåìÿ, #ñåíñîðà, ïîêàçàíèå
  # ãåíåðèðóåì ïîêàçàòåëè âëàæíîñòè, äîïóñòèìûé äèàïàçîí [0; 100]
  n <- length(tick.seq)
  mydata <- data.frame(name = rep(sensor$name, each = n),
                       lon = rep(sensor$lon, each = n),
                       lat = rep(sensor$lat, each = n),
                       type = "soil_moisture",
                       location = "Moscow Lab",
                       #location = "Êàðòîôåëü 1",
                       timestamp = tick.seq, 
                       value = rescale(rnorm(nrow(sensor) * n, 70, 30), to = c(50, 80)))
  mydata <- mydata %>%
    mutate(min = value * runif(nrow(sensor) * n, 0.1, 0.9)) %>%
    mutate(max = value * runif(nrow(sensor) * n, 1.1, 1.5)) %>%
    mutate(max = ifelse(max <= 100, max, 100))
    
  # rnorm(1000, 3, .25) # Generates 1000 numbers from a normal with mean 3 and sd=.25
  
  # äëÿ ñîîòâ. ðåàëèÿì ñäåëàåì ðàçáðîñ âî âðåìåíè èçìåðåíèÿ
  # mydata$timestamp <- mydata$timestamp + runif(nrow(mydata), min = -5*60, max = 5*60)
  mydata$value <- round(mydata$value + 0.2*sin(as.numeric(mydata$timestamp)/86400*(0.1*pi)), 1)
  
  mydata
}


flog.appender(appender.file("./log/iot.log"))
flog.threshold(TRACE)
flog.info("Job started")
flog.info("Working directory: %s", getwd())

flog.info("Processing started")
df <- generate_field_data(back_days = 7, forward_days = 3)
qplot(timestamp, value, data = df)
# http://stackoverflow.com/questions/25550711/convert-data-frame-to-json
x <- jsonlite::toJSON(df, pretty = TRUE)
write(x, file="./export/sample.json")
flog.info("Job finished")


# ñãåðåíèðóåì json ïîä òðåáîâàíèÿ Ïàøè ---------------------------------------------------
# http://arxiv.org/pdf/1403.2805v1.pdf  |   http://arxiv.org/abs/1403.2805
df2 <- data.frame(timestamp = round(as.numeric(df$timestamp), 0), 
                  soil_moisture = round(df$value, 1),
                  soil_moisture_min = round(df$min, 1), 
                  soil_moisture_max = round(df$max, 1)) %>%
  arrange(timestamp)

x <- jsonlite::toJSON(list(results = df2), pretty = TRUE)
write(x, file="./export/sample_highcharts.json")


stop() # èçûñêàíèÿ
# ñãåðåíèðóåì json ïîä òðåáîâàíèÿ Ïàøè ---------------------------------------------------
# http://arxiv.org/pdf/1403.2805v1.pdf  |   http://arxiv.org/abs/1403.2805
options(stringsAsFactors = FALSE)
df1 <- data.frame(results = rep('name', nrow(df)))
df2 <- data.frame(timestamp = round(as.numeric(df$timestamp), 0), 
                  soil_moisture = round(df$value, 1),
                  soil_moisture_min = round(df$min, 1), 
                  soil_moisture_max = round(df$max, 1)) %>%
  arrange(timestamp)

# Îáÿçàòåëüíî íàäî óðîâíÿòü êîëè÷åñòâî ýëåìåíòîâ, èíà÷å ïîëó÷àåì îøèáêó: 
# Error in `$<-.data.frame`(`*tmp*`, "results", value = list(timestamp = c(1463345834,  : replacement has 671 rows, data has 1
df1$results <- df2 

ll <- list(results = df2)

# df3 <- data.frame(results = df2) # òàê âñòðîåííûé data.frame íå ïîëó÷èì

gp <- ggplot(df1$results, aes(timestamp, soil_moisture)) +
  geom_line()
gp

x <- jsonlite::toJSON(ll, pretty = TRUE)
write(x, file="./export/sample_highcharts.json")


