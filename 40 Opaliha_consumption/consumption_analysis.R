library(lubridate)
library(ggplot2)

# http://stackoverflow.com/questions/5796924/how-can-i-determine-the-current-directory-name-in-r
getwd()
setwd("R:/Ilya/Опалиха/потребление_ресурсов")  # note / instead of \ in windows 
mydata <- read.table("gas_consumption.csv", header=TRUE, stringsAsFactors = FALSE,
                     sep=";")

str(mydata)
mydata$date <- dmy(mydata$date)
str(mydata)

# сделаем выборку по датам
# http://stackoverflow.com/questions/17244077/select-subset-by-date-in-r
# subdata <- mydata[ymd(20121201) < mydata$date & mydata$date < ymd(20130101),]
subdata <- mydata[ymd(20120101) < mydata$date,]

# альтернативный вариант -- subset
# http://stackoverflow.com/questions/8992585/creating-a-data-frame-based-on-day-of-week-from-a-data-frame-containing-several

# geom_point docs: http://docs.ggplot2.org/0.9.3.1/geom_point.html
# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_point

p <- ggplot(subdata, aes(x=date, y=balance_m3))
p <- ggplot(subdata, aes(x=date, y=balance_rub), size=I(5))
p <- p + 
  geom_point(size=4, shape=21) + # produce scatterplot # fill="green",
  xlab("Дата") +
  ylab("Баланс, руб") +
  geom_line() + 
  stat_smooth(aes(outfit=fit<<-..y..)) #fitting http://stackoverflow.com/questions/9789871/method-to-extract-stat-smooth-line-fit

p
#======

x <- c("2010-04-14-04-35-59", "2010-04-01-12-00-00")
str(ymd_hms(x))



setClass('myDate')
setAs("character","myDate", function(from) ymd_hms(from) )

tmp <- c("1, 15/08/2008", "2, 23/05/2010")
con <- textConnection(tmp)

tmp2 <- read.csv(con, colClasses=c('numeric','myDate'), header=FALSE)
str(tmp2)