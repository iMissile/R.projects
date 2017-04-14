# исследуем суточные агрегаты
library(lubridate)
library(ggplot2)
library(scales)
library(forecast)
library(stringr)
library(RColorBrewer)
#library(microbenchmark)
library(dplyr)
library(magrittr)
library(xts)

par(ask = FALSE) # выключили паузу при выводе графиков
# http://stackoverflow.com/questions/5796924/how-can-i-determine-the-current-directory-name-in-r
#getwd()
# setwd("C:/iwork.HG/R.projects/02 predictive_PM_demo/data3")  # note / instead of \ in windows 
filename = "interface_max_load.v1.csv"
filename = "./data3/interface_max_load.v2.csv"
filename = "./data_m/simple_20150321_20150610.csv"

mydata <- read.table(filename, header=TRUE, stringsAsFactors = FALSE,
                     sep=";") # comment.char = "#"
#mydata$date <- mdy_hm(mydata$date) #12/27/2013 3:00
#mydata$date <- dmy_hms(mydata$date, truncated = 3) #01.06.2013
mydata$date <- ymd_hms(mydata$date, truncated = 3) #01.06.2013

# очистка таблицы http://www.r-bloggers.com/using-r-reading-tables-that-need-a-little-cleaning/
# http://stackoverflow.com/questions/9236438/how-do-i-run-apply-on-a-data-table
mydata$util_out_max <- as.numeric(str_replace(mydata$util_out_max,"%",""))
# выборка и анализ подмножества
startdate <- ymd("2015-03-21") # ymd(startdate) # yday(startdate)
# http://www.jstatsoft.org/v40/i03/paper "Dates and Times Made Easy with lubridate"
enddate <- startdate + months(2)

#subdata <- mydata[startdate < mydata$date & mydata$date < enddate,]
# делаем через dplyr
subdata <- filter(mydata, startdate < date & date < enddate)
#sensor <- window(ts(subdata$util_out_max, start=c(year(startdate), yday(startdate)), frequency=7),
#             start=c(year(startdate), yday(startdate)), 
#             end=c(year(startdate), yday(startdate)+90)) # consider adding a start so you get nicer labelling on your chart.

# строим формальный сенсор без прив€зки к дате, с указанием недельной периодичности
# почему берем 7 сказано в "Forecasting with daily data" http://robjhyndman.com/hyndsight/dailydata/
sensor <- ts(subdata$util_out_max, frequency=14)
sensor

#  ARIMA = Auto Regressive Integrated Moving Average
# увы, ARIMA работает только дл€ суточных измерений с глубиной не более 350 измерений
# ограничение в 350: http://robjhyndman.com/hyndsight/longseasonality/
# ==============================================
fit <- auto.arima(sensor) #http://stackoverflow.com/questions/14331314/time-series-prediction-of-daily-data-of-a-month-using-arima
# fit <- tbats(sensor)
fcast <- forecast(fit, h=7) # h - Number of periods for forecasting

# Directly plot your forecast without your axes
plot(fcast)

# ======================= переходим к ручному рисованию ==========================
# св€зано это с тем, что дл€ прогноза надо указывать недельный период, а данные указываютс€ в формате год.дробь
# иcпользованные материалы:
# http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/
# http://blog.rstudio.org/2012/09/07/ggplot2-0-9-2/


# структура класса ts:
# structure(c(x1, x2, ... ), .Tsp = c(1978.25, 1986.5, 4), class = "ts")

# пытаемс€ вытащить все данные из смоделированной последовательности, окончание смещаетс€ на глубину прогнозировани€

lenx <- length(fcast$x)
lenmn <- length(fcast$mean)
# превращаем теперь в дневную последовательность
# http://knowledgestockpile.blogspot.ru/2011/12/creating-sequence-of-dates-in-r.html
# dates <- seq(as.Date("2011-01-05"), as.Date("2011-03-15"), by=1)
# dates <- seq(as.Date("2011-01-05"), by=1, len=45)
realtime <- seq(as.Date(startdate), by=1, len=lenx+lenmn)

df <- data.frame(time=realtime,
                 x=c(fcast$x, fcast$mean),
                 forecast=c(rep(NA, lenx), fcast$mean),
                 low1=c(rep(NA, lenx), fcast$lower[, 1]),
                 upp1=c(rep(NA, lenx), fcast$upper[, 1]),
                 low2=c(rep(NA, lenx), fcast$lower[, 2]),
                 upp2=c(rep(NA, lenx), fcast$upper[, 2])
)

cpal <- brewer.pal(7, "Blues")
cpal <- brewer.pal(7, "Oranges")

# точки превышени€
hard_threshold = 70
#df_upper <- data.frame(time=df$time[df$x>hard_threshold],
#                       value=df$x[df$x>hard_threshold])
# dplyr
df_upper <- filter(df, x > hard_threshold) %>% rename(value=x)

dfplot <- ggplot(df, aes(time, x)) +
  #geom_ribbon(aes(ymin=low2, ymax=upp2), fill="yellow") +
  #geom_ribbon(aes(ymin=low1, ymax=upp1), fill="orange") +
  geom_ribbon(aes(ymin=low2, ymax=upp2), fill=cpal[1]) +
  geom_ribbon(aes(ymin=low1, ymax=upp1), fill=cpal[2]) +
  theme_bw() +
  geom_line() +
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

dfplot

# каким образом перейти в директорию уровнем выше?
# setwd("../")
# Open a new png device to print the figure out to (or use tiff, pdf, etc).
png(filename = "figure_x.png", width = 1200, height = 600, units = 'px')
print(dfplot) #end of print statement
dev.off() #close the png device to save the figure. 
# ========================================

dfe <- df
# BASE Renaming columns in a data frame
# http://www.cookbook-r.com/Manipulating_data/Renaming_columns_in_a_data_frame/
#names(dfe)[names(dfe)=="low1"] <- "Lo80"
#names(dfe)[names(dfe)=="low2"] <- "Lo95"
#names(dfe)[names(dfe)=="upp1"] <- "Hi80"
#names(dfe)[names(dfe)=="upp2"] <- "Hi95"

# http://stackoverflow.com/questions/21502465/replacement-for-rename-in-dplyr
# http://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

#system.time(
#microbenchmark(
dfe <- dplyr::rename(dfe, Lo80=low1)
dfe <- dplyr::rename(dfe, Lo95=low2)
dfe <- dplyr::rename(dfe, Hi80=upp1)
dfe <- dplyr::rename(dfe, Hi95=upp2)



# Write CSV in R
# http://rprogramming.net/write-csv-in-r/
write.table(dfe, file = "dfe.csv", row.names=FALSE, na="", col.names=TRUE, sep=",")


#==================== экcперименты с TimeSeries ============
# http://stackoverflow.com/questions/8732461/convert-data-frame-with-date-column-to-timeseries
ets <- xts(subdata$util_out_max, subdata$date)
# раскладываем на компоненты
# http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
# ets_components <- decompose(ets) # не работает, не указан период

#==================== экcперименты с поиском аномалий ============
library(AnomalyDetection)
# help(AnomalyDetectionTs)
# help(AnomalyDetectionVec)
# data(raw_data)
# res = AnomalyDetectionTs(raw_data, max_anoms=0.02, direction='both', only_last="day", plot=TRUE)
res = AnomalyDetectionTs(subdata, max_anoms=0.03, direction='both', plot=TRUE)
res$plot
