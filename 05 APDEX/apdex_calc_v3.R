# Формула вычисления APDEX
library(tidyverse)
library(dplyr)
library(magrittr)
library(lubridate)
#library(scales)
#library(forecast)
library(stringr)
library(hrbrthemes)
#library(RColorBrewer)

# gf <- c(4,5,6)
# tracemem(gf)
# 
# adf <- function(df){
#   tracemem(df)
# пока не модифицируем df, объект идентичен родителю. Проверяем с помощью tracemem()
#   browser()
#   df <- 1
#   browser()
# }
# 
# adf(gf)


#' @timestamp -- временная метка
#' @t_resp -- время отклика
#' @type -- категория времени отклика
#' @cur_time -- текущее время
#' @win_size -- размер временного окна минутах от момента измерения в прошлое
apdexf <- function(df, cur_time, win_size) {

  # Разбиваем, все выполненные операции на 3 категории:
  # N  – общее количество произведенных операций
  # NS - количество итераций, которые выполнены за менее чем целевое время [0 – Т]
  # NF – количество операция, которые выполнены за (Т – 4Т] 
  # (т.е от целевого времени до целевого времени умноженного на 4)
  # Индекс APDEX = (NS + NF/2)/N.
  # print(dplyr::location(df))
  # filter может и хорошо, но заставляет копировать данные
  # с точки зрения скорости оптимально просто отмаркировать доп. колонку
  t_df0 <- df %>%
    filter(timestamp>cur_time-minutes(win_size) && timestamp<=cur_time)
  
  t_df <- t_df0 %>%
    group_by(type) %>%
    summarize(cnt=n())

  print(dplyr::changes(t_df0, t_df))
  
  
  browser()
  
  # print(t_df)
  # print(object.size(t_df))

  
#apdex_NS <- t_df$cnt[[t_df$type=="S"]]
#apdex_NF <- t_df$cnt[[t_df$type=="F"]]
  #respw
  
  #apdex_N  <- length(respw$response_time) #APDEX_total
  #apdex_NS <- length(which(respw$response_time < T_agreed)) # APDEX_satisfied
  #apdex_NF <- length(which(T_agreed < respw$response_time & respw$response_time < 4*T_agreed)) # APDEX_tolerated F=4T
  

  apdex_NS <- t_df %>% filter(type==0) %>% "[["("cnt")
  apdex_NF <- t_df %>% filter(type==1) %>% "[["("cnt")
  apdex_NO <- t_df %>% filter(type==2) %>% "[["("cnt")
  
  # если переменной нет, то получаем integer(0), руками превращаем в нули
  apdex_NS <- ifelse(length(apdex_NS) == 0, 0., apdex_NS)
  apdex_NF <- ifelse(length(apdex_NF) == 0, 0., apdex_NF)
  apdex_NO <- ifelse(length(apdex_NO) == 0, 0., apdex_NO)
  
  apdex_N  <- apdex_NO + apdex_NF + apdex_NS
  apdex <- (apdex_NS + apdex_NF/2)/apdex_N 
  return(apdex)
}


apdexf2 <- function(df, window_start, cur_time) {
  
  # Разбиваем, все выполненные операции на 3 категории:
  # N  – общее количество произведенных операций
  # NS - количество итераций, которые выполнены за менее чем целевое время [0 – Т]
  # NF – количество операция, которые выполнены за (Т – 4Т] 
  # (т.е от целевого времени до целевого времени умноженного на 4)
  # Индекс APDEX = (NS + NF/2)/N.
  # print(dplyr::location(df))
  # filter может и хорошо, но заставляет копировать данные
  # с точки зрения скорости оптимально просто отмаркировать доп. колонку
  # t_df <- df %>%
  #   mutate(in_window=if_else(timestamp>window_start && 
  #                              timestamp<=cur_time, 1, 0))

  # чем меньше присваиваний (любых), тем быстрее код
  # filter получается в 2 раза быстрее и меньше памяти ест, чем разметка
  # данных не очень много, поэтому доп. select в цикле только время ест
  # browser()

  # f_expr <- paste0("timestamp > ", window_start, " && timestamp <= ", cur_time)
  # с POSIXct печать не получается
  # browser()
  t_df <- df %>%
    # filter(timestamp>window_start && timestamp<=cur_time) %>%
    # filter_(.dots=f_expr) %>%
    filter_(lazyeval::interp(~ timestamp>var, var=as.name("window_start"))) %>%
    filter_(lazyeval::interp(~ timestamp<=var, var=as.name("cur_time")))
    # filter_(lazyeval::interp(~ timestamp>start && timestamp<=end, 
    #                          .values=list(start=quote(window_start), 
    #                                       end=quote(cur_time)))) %>%
    # summarise_at(vars(satisfied, tolerated, frustrated), sum)
    # mutate(apdex=(satisfied + tolerated/2)/
    #          (satisfied + tolerated + frustrated))
  
  # так быстрее на порядок, чем summarise
  s <- sum(t_df$satisfied)
  t <- sum(t_df$tolerated)
  f <- sum(t_df$frustrated)
  # browser()
  
  #print(dplyr::changes(t_df0, t_df))
  # apdex <- with(t_df, {(satisfied + tolerated/2)/
  #     (satisfied + tolerated + frustrated)})
  
  # apdex <- (t_df$satisfied + t_df$tolerated/2)/
  #   (t_df$satisfied + t_df$tolerated + t_df$frustrated)
  
  (s + t/2)/(s + t + f)
}

# http://stackoverflow.com/questions/5796924/how-can-i-determine-the-current-directory-name-in-r
# getwd()
# setwd("./data")  # note / instead of \ in windows 
filename = "./data/RUM_simulated_data.csv"

mydata <- read_delim(filename, delim=';') %>%
  mutate(timestamp=dmy_hms(timestamp)) #01.06.2013

# Разбиваем, все выполненные операции на 3 категории:
# N  – общее количество произведенных операций
# 0 NS - количество итераций, которые выполнены за менее чем целевое время [0 – Т]
# 1 NF – количество операция, которые выполнены за (Т – 4Т] 
# 2 - все прочие операции 
# (т.е от целевого времени до целевого времени умноженного на 4)
# Индекс APDEX = (NS + NF/2)/N.
t_agreed <- 0.7
mydata %<>% rename(t_resp=response_time) %>%
  mutate(type=case_when(
    .$t_resp <= t_agreed ~ 0,
    .$t_resp <= 4*t_agreed ~ 1,
    TRUE ~ 2))


# обрабатывать type в цикле оказывается очень накладно. Делаем отдельные вектора
mydata %<>%
  select(-type) %>%
  filter(t_resp > 0) %>% # отбросили потенциальный шлак
  mutate(satisfied=if_else(t_resp <= t_agreed, 1, 0)) %>%
  mutate(frustrated=if_else(t_resp > 4*t_agreed, 1, 0)) %>%
  mutate(tolerated=1-satisfied-frustrated) %>%
  mutate(control=satisfied+tolerated+frustrated) %>%
  mutate(window_start=timestamp-minutes(15)) %>%
  select(window_start, everything())
  
time_df <- mydata %>%
  select(window_start, timestamp)

mydata %<>%
  select(timestamp, satisfied, tolerated, frustrated)


# а теперь считаем apdex

#aa <- map2(time_df$window_start, time_df$timestamp,
#                     ~ apdexf2(mydata, .x, .y))

mydata$apdex <- map2(time_df$window_start, time_df$timestamp,
                     ~ apdexf2(mydata, .x, .y)) %>%
  unlist()

stop()
# нарисуем график
# qplot(timestamp, response_time, data = mydata, geom = c("point", "smooth"))

# определяем временнЫе рамки
start_time <- min(mydata$timestamp)
end_time <- max(mydata$timestamp)

# поставить фиксированные границы + цветные горизонтальные линии
qplot(timestamp, apdex, data = mydata, geom = c("point", "smooth"))

# windowsFonts(Verdana="TT Verdana")
windowsFonts(robotoC="Roboto Condensed")

dfplot <- ggplot(mydata, aes(timestamp, apdex)) +
  #scale_x_date(labels = date_format("%d %b %Y"), breaks = date_breaks("week")) +
  # scales http://stackoverflow.com/questions/3606697/how-to-set-limits-for-axes-in-ggplot2-r-plots
  theme_ipsum_rc(base_family="robotoC", base_size = 11.5) +
  # theme_ipsum() + 
  # theme_bw(base_size = 12, base_family = "") +
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
  # geom_density(alpha = 0.3) +
  xlab("Дата") +
  ylab("APDEX")
  
dfplot

# каким образом перейти в директорию уровнем выше?
#setwd("../")
# Open a new png device to print the figure out to (or use tiff, pdf, etc).
png(filename = "figure_apdex.png", width = 1200, height = 600, units = 'px')
print(dfplot) #end of print statement
dev.off() #close the png device to save the figure. 

stop("Скрипт завершен")
# ============================================
# https://github.com/wch/extrafont

# Vector of font family names
fonts()

# Show entire table
fonttable()

# ============================================

geom_line(data=df[!is.na(df$forecast), ], aes(time, forecast), color="blue", size=1.5, na.rm=TRUE) +
  # http://docs.ggplot2.org/current/scale_continuous.html
  #scale_x_continuous("") +
  #scale_y_continuous(limits=c(0, 120)) +
  # scale_x_date(breaks=date_breaks('days'), labels = date_format("%d %b %Y")) +
  # ОБЯЗАТЕЛЬНО library(scales)
  # http://docs.ggplot2.org/current/scale_date.html
  scale_x_date(labels = date_format("%d %b %Y"), breaks = date_breaks("week")) +
  geom_hline(aes(yintercept=100), colour="red", linetype="dashed", size=1) +
  geom_hline(aes(yintercept=hard_threshold), colour="magenta", linetype="dashed", size=1) +
  geom_vline(xintercept=as.numeric(df_upper$time), colour="green", linetype="dashed", size=1) + # linetype="dotted"
  #geom_point(data=df_upper, colour = "green", size = 4, aes(x=time, y=value), shape=1) +
  geom_point(data=df_upper, colour = "green", size = 5, aes(x=time, y=value)) +
  geom_point(data=df_upper, colour = "grey90", size = 3, aes(x=time, y=value)) +
  xlab("Дата") +
  ylab("Загрузка CPU, %") +
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
