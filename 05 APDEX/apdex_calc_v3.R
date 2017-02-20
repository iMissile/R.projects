# ‘ормула вычислени€ APDEX
library(lubridate)
library(ggplot2)
#library(scales)
#library(forecast)
library(stringr)
#library(RColorBrewer)

apdexf <- function(respw, T_agreed = 1.2) {
  # http://stackoverflow.com/questions/3818147/count-number-of-vector-values-in-range-with-r
  which(0 < respw$response_time & respw$response_time < T_agreed)
  
  # –азбиваем, все выполненные операции на 3 категории:
  # N  Ц общее количество произведенных операций
  # NS - количество итераций, которые выполнены за менее чем целевое врем€ 0 Ц “
  # NF Ц количество операци€, которые выполнены за “ Ц 4“ 
  # (т.е от целевого времени до целевого времени умноженного на 4)
  # »ндекс APDEX = (NS + NF/2)/N.
  
  respw
  
  apdex_N  <- length(respw$response_time) #APDEX_total
  apdex_NS <- length(which(respw$response_time < T_agreed)) # APDEX_satisfied
  apdex_NF <- length(which(T_agreed < respw$response_time & respw$response_time < 4*T_agreed)) # APDEX_tolerated F=4T
  
  apdex <- (apdex_NS + apdex_NF/2)/apdex_N 
  return(apdex)
}


# http://stackoverflow.com/questions/5796924/how-can-i-determine-the-current-directory-name-in-r
getwd()
setwd("C:/iwork.HG/R.projects/05 APDEX/data")  # note / instead of \ in windows 
filename = "RUM_simulated_data.csv"

mydata <- read.table(filename, header = TRUE, stringsAsFactors = FALSE,
                     sep=";") # comment.char = "#"
#mydata$date <- mdy_hm(mydata$date) #12/27/2013 3:00
mydata$timestamp <- dmy_hms(mydata$timestamp) #01.06.2013

# нарисуем график
# qplot(timestamp, response_time, data = mydata, geom = c("point", "smooth"))
#### qplot(timestamp, response_time, data = mydata)

# определ€ем временнџе рамки
# http://stackoverflow.com/questions/77434/how-to-access-the-last-value-in-a-vector
# http://stat.ethz.ch/R-manual/R-patched/library/utils/html/head.html
start_time <- head(mydata, 1, addrownums = FALSE)$timestamp
end_time <- tail(mydata, 1, addrownums = FALSE)$timestamp

# необходимо посчитать количество интервалов и сделать вектор значений подсчета
# duration считает в секундах рассто€ние между двум€ точками
# http://www.jstatsoft.org/v40/i03/paper "Dates and Times Made Easy with lubridate"
dt = dminutes(15)

# http://stackoverflow.com/questions/10689055/create-an-empty-data-frame
# df.apdex = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("Date", "File", "User"))), stringsAsFactors=F)
# http://stackoverflow.com/questions/12613909/how-to-create-empty-data-frame-with-column-names-specified-in-r
# df.apdex = data.frame(timestamp=NA, apdex=NA)[numeric(0), ]

# R Cookbook: 5.21 Preallocating a Data Frame
# df.apdex = data.frame(timestamp=structure(numeric(0), class = c("POSIXt", "POSIXct"), tzone = "UTC"), apdex=numeric(0))
df.apdex = data.frame(timestamp=numeric(0), apdex=numeric(0))

for(t in seq(from = start_time, by = dt, length.out = floor(as.duration(end_time - start_time)/dt))){
  # выборка и анализ подмножества
  respw <- subset(mydata, t <= timestamp & timestamp < t+dt)
  # str(respw)
  # str(origin + seconds(t))
  # cat(origin + seconds(t), '  ', apdexf(respw), '\n\n')
  # R Cookbook: 5.20 Appending Rows to a Data Frame
  df.apdex <- rbind(df.apdex, data.frame(timestamp = t, apdex = apdexf(respw)))
}

df.apdex$timestamp <- origin + seconds(df.apdex$timestamp)
# поставить фиксированные границы + цветные горизонтальные линии
qplot(timestamp, apdex, data = df.apdex, geom = c("point", "smooth"))


dfplot <- ggplot(df.apdex, aes(timestamp, apdex)) +
  #scale_x_date(labels = date_format("%d %b %Y"), breaks = date_breaks("week")) +
  # scales http://stackoverflow.com/questions/3606697/how-to-set-limits-for-axes-in-ggplot2-r-plots
  ylim(0, 1) +
  # http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
  # APDEX индикаторы/ 0.94, 0.85, 0.7, 0.5
  geom_ribbon(aes(ymin=0.94, ymax=1, alpha = 0.1), fill="paleturquoise") +
  geom_ribbon(aes(ymin=0.85, ymax=0.94, alpha = 0.1), fill="palegreen") +
  geom_ribbon(aes(ymin=0.7, ymax=0.85, alpha = 0.1), fill="palegoldenrod") +
  geom_ribbon(aes(ymin=0.5, ymax=0.7, alpha = 0.1), fill="palevioletred1") +
  # geom_hline(aes(yintercept=.94), colour="blue", linetype="dashed", size=1) +
  geom_hline(aes(yintercept=.85), colour="green", linetype="dashed", size=1) +
  geom_hline(aes(yintercept=.7), colour="yellow", linetype="dashed", size=1) +
  geom_hline(aes(yintercept=.5), colour="red", linetype="dashed", size=1) +
  geom_line() +
  theme_bw(base_size = 12, base_family = "") +
  # geom_density(alpha = 0.3) +
  xlab("ƒата") +
  ylab("APDEX")
  
dfplot

# каким образом перейти в директорию уровнем выше?
setwd("../")
# Open a new png device to print the figure out to (or use tiff, pdf, etc).
png(filename = "figure_apdex.png", width = 1200, height = 600, units = 'px')
print(dfplot) #end of print statement
dev.off() #close the png device to save the figure. 

stop("—крипт завершен")

# ============================================

geom_line(data=df[!is.na(df$forecast), ], aes(time, forecast), color="blue", size=1.5, na.rm=TRUE) +
  # http://docs.ggplot2.org/current/scale_continuous.html
  #scale_x_continuous("") +
  #scale_y_continuous(limits=c(0, 120)) +
  # scale_x_date(breaks=date_breaks('days'), labels = date_format("%d %b %Y")) +
  # ќЅя«ј“≈Ћ№Ќќ library(scales)
  # http://docs.ggplot2.org/current/scale_date.html
  scale_x_date(labels = date_format("%d %b %Y"), breaks = date_breaks("week")) +
  geom_hline(aes(yintercept=100), colour="red", linetype="dashed", size=1) +
  geom_hline(aes(yintercept=hard_threshold), colour="magenta", linetype="dashed", size=1) +
  geom_vline(xintercept=as.numeric(df_upper$time), colour="green", linetype="dashed", size=1) + # linetype="dotted"
  #geom_point(data=df_upper, colour = "green", size = 4, aes(x=time, y=value), shape=1) +
  geom_point(data=df_upper, colour = "green", size = 5, aes(x=time, y=value)) +
  geom_point(data=df_upper, colour = "grey90", size = 3, aes(x=time, y=value)) +
  xlab("ƒата") +
  ylab("«агрузка CPU, %") +
  # scale_shape(solid = FALSE) +
  # http://blog.rstudio.org/2012/09/07/ggplot2-0-9-2/  
  # opts(title=paste("Forecasts from ", fcast$method))
  labs(title=paste("Forecasts from ", fcast$method))


# http://stackoverflow.com/questions/3505701/r-grouping-functions-sapply-vs-lapply-vs-apply-vs-tapply-vs-by-vs-aggrega
myFun <- function(x, data, dt) {
  dput(dt)
  return (subset(data, x <= timestamp & timestamp < x+dt))
}

rr <- myFun(cur_time, mydata, dt)


res <- sapply(mpoint, function(x, data = mydata) 
  #{cat(x, ' -- ') str(subset(data, x <= timestamp & timestamp < x+dminutes(15)); cat('\n')} )
{str(subset(data, x <= timestamp & timestamp < x+dminutes(15)))} )
res
#res <- sapply(mpoint, function(x, data = mydata) {x+1})

apdexf(respw)



qplot(timestamp, response_time, data = respw, geom = c("point", "smooth"))


cur_time <- start_time + dminutes(60) # minutes(60)
as.duration(end_time - start_time)

# subset(mydata, cur_time %<=% timestamp %<% cur_time + minutes(1))
# subdata <- mydata[cur_time < mydata$timestamp & mydata$timestamp < cur_time+minutes(10),]
respw <- subset(mydata, cur_time <= timestamp & timestamp < cur_time + dminutes(5))
respw

# осталось сделать цикл по окошкам
mpoint
# mydata
#sapply(mpoint, function(x) {mean(x)})
