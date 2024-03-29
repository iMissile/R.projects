# rm(list=ls()) # ������� ��� ����������
# R & Github little_tricks    https://github.com/bdemeshev/em301/wiki/little_tricks

#library(plyr)
#library(tidyverse)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)
#library(forecast)
library(stringr)
library(RColorBrewer)
library(wesanderson) # https://github.com/karthik/wesanderson
library(microbenchmark)
library(reshape2)
library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(purrr)
library(tibble)
library(xts)
library(zoo)
library(caTools)
# library(xlsx) # :( rJava - ������: No CurrentVersion entry in Software/JavaSoft registry! Try re-installing Java and make sure R and Java have matching architectures
library(jsonlite)
#library(logging)
library(futile.logger) # ���� � logging
library(broom)

options(warn = 2)

flog.appender(appender.file('baseline.log'))
# flog.threshold(TRACE)

source("funcs.R") # ��������� ����������� �������, http://adv-r.had.co.nz/Functions.html
# R Performance. http://adv-r.had.co.nz/Performance.html

# �������� ������ �� ����� �������� �������������� �������� � ������������� �������� (������ ������) ���
# ������������ ���������� ������ (25 ���������)

# ��� ��������� ��������, ��������� ������� ��� ����������������� �������. 
# ���� ����� ���, �� ���������� ��������� ������ �� ������ ����� � ���������� �� � ���� ������� � ����
# http://stackoverflow.com/questions/21370132/r-data-formats-rdata-rda-rds-etc
# http://www.fromthebottomoftheheap.net/2012/04/01/saving-and-loading-r-objects/

rawdata.filename = "./data/bandwidth.csv" # �������� ����������� � %, ��������� 1 �������
objdata.filename = "subdata.rds"

if (!file.exists(objdata.filename)){
  # ����� �� ����������, �������� ������� �� ����� ������ � ���������� ������ � ����
  # loginfo('loading file %s', rawdata.filename)
  flog.info(str_c("loading file ", rawdata.filename))
  rawdata <- read_csv(rawdata.filename) # http://barryrowlingson.github.io/hadleyverse/#5
  problems(rawdata)
  
  # �������� �������������� �������
  # ������� delta_time -- �������� �������� �� eHealth, ���������� ������� ����� �����������
  s_date <- ymd("2015-04-01", tz = "Europe/Moscow")
  e_date <- s_date + days(60)

  t.df <- rawdata %>%
    mutate(timestamp = dmy_hms(rawdata$timestamp, truncated = 3, tz = "Europe/Moscow")) %>% #��������� ��������� ������� ������
    filter(s_date < timestamp & timestamp < e_date) %>%
    tidyr::gather(`bandwidth_in`, `bandwidth_out`, key = "direction", value = "value") %>% # https://github.com/hadley/tidyr/issues/231
    select(timestamp, direction, value)

  # �������� ������ ��� �������
  # ��� �������� ��������� ������� ���������� � ���� 5-�� �������� ���������� 
  df0 <- generate.discrete(t.df %>% filter(direction == "bandwidth_in"))

  # ��� ��������� � ��������� ���� �������� ������ �� ����
  # http://stackoverflow.com/questions/18503177/r-apply-function-on-specific-dataframe-columns
  # round_date -- ��������� � ��� �������, �� ����� ��������, ��� ���� ��������� ����.
  # + ������� ��������� �� ������� ����������
  # http://stackoverflow.com/questions/10705328/extract-hours-and-seconds-from-posixct-for-plotting-purposes-in-r
  # using lubridate::hour and lubridate::minute
  
set.wheight <- function(x) {
  case_when(x < 5 ~ 0,
            x < 10 ~ 1,
            x < 20 ~ 2,
            x < 40 ~ 3,
            x >= 40 ~ 4)
    }
  
  df1 <- df0 %>%
    mutate(date = floor_date(timestamp, "day")) %>%
    mutate(textdate = as.factor(format(date, format = "%d.%m (%a)"))) %>%
    mutate(nwday = wday(timestamp)) %>%
    mutate(joined_wday = ifelse(nwday == 3 | nwday == 5, 4, nwday)) %>% # �������, ��� �������-������� ������������ ������
    mutate(hgroup = hgroup.enum(timestamp)) %>%
    mutate(ticks = time.ticks.enum(timestamp)) %>%
    # mutate(weight = set.wheight(value)) %>% # ��� ��������. ��� ������ ��������� ��� ������� ��������
    # mutate(weight = round(value/20)) %>% # ��� ��������. ��� ������ ��������� ��� ������� ��������
    mutate(weight = log10(value + 1e-5)) %>% # ��� ��������. ��� ������ ��������� ��� ������� ��������
    mutate(weight = ifelse(weight < log10(5), 0, weight)) %>% # ���� ������� ����� ��������
    mutate(baseline = NA_real_)

    # subdata$textdate <- as.factor(subdata$textdate)

    # ����������� ���������� ������������� ����������
  integr.df <- sum_per_day(df1)
  # � ���������� � �������� ������
  subdata <- df1 %>%
    left_join(integr.df, by = "date") %>%
    mutate(ratio = value / mean_value) # ��������� ���������, ������� ���������� ���������� ����� � ���
  
  # ��������� ������
  saveRDS(subdata, objdata.filename) #, compress = FALSE, ascii = TRUE)
} else {
  subdata <- readRDS(objdata.filename)
}

# ����� ��������� ������� 'subdata.f(iltered)', ����������� ���������� �� ��������� ����
# ����� ���������� �� ���� ������. 1 - �����������, 7 - �������
days.stat <- subdata %>%
  group_by(nwday) %>%
  summarise(mean = mean(integr), sd = sd(integr)) %>%
  mutate(i_min = mean - 1.7 * sd) %>%
  mutate(i_max = mean + 1.7 * sd) %>%
  select(-mean, -sd) %>%
  arrange(nwday)

# ������� ������ � ���������
slicedata <- subdata %>%
  left_join(days.stat, by = "nwday") %>%
  filter(integr >= i_min & integr <= i_max)

# ===============================

# � ����� ��������� ���������� � randomForest
# http://theanalyticalminds.blogspot.ru/2015/04/part-4a-modelling-predicting-amount-of.html
library(randomForest)

# ����� ��� �������� ������������ ������������
train <- dplyr::sample_frac(slicedata, 0.7, replace = FALSE) # replace = TRUE -- ��������� ������ ������������� �������. = FALSE -- ���
# ��� ��������� ���������� ��� ������������
test <- dplyr::anti_join(slicedata, train)

# === ��� ��������� ����� ��� �� ����� ������, ����� �������������� ������� � �������
# ���� ������ �� �������, �� ��� ����, ������� �� ������ �� ������ �� �������, �� � ��������� ���������� ���������
dt <- max(slicedata$timestamp) - min(slicedata$timestamp)
check_time = min(slicedata$timestamp) + 0.7*dt

train <- dplyr::filter(slicedata, timestamp <= check_time)
# ��� ��������� ���������� ��� ������������
test <- dplyr::anti_join(slicedata, train)


# ��� ������������� ����������
set.seed(123)

# ������� randomfForest � 1000 ���������
rf <- randomForest(ratio ~ joined_wday + nwday + ticks + weight, data = train, importance = TRUE, ntree = 1000)

# How many trees are needed to reach the minimum error estimate? 
# This is a simple problem; it appears that about 100 trees would be enough.
which.min(rf$mse)
# Plot rf to see the estimated error as a function of the number of trees
# (not running it here)
# plot(rf)

# ��������� �� 1-�� ������ � ����: http://bdemeshev.github.io/r_cycle/cycle_files/22_forest.html
tree1 <- getTree(rf, 1, labelVar = TRUE)
head(as_tibble(tree1))

# Using the importance()  function to calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf)[,1], decreasing = TRUE), optional = T)
names(imp) <- "% Inc MSE"
imp

# As usual, predict and evaluate on the test set
test.pred.forest <- predict(rf, test)
RMSE.forest <- sqrt(mean((test.pred.forest - test$value)^2))
RMSE.forest

MAE.forest <- mean(abs(test.pred.forest - test$value))
MAE.forest

# === ��������� ������ ���������� �������� � ���������� �������

#test <- subdata
test$baseline <- predict(rf, test)

test.df <- test %>%
  select(-value) %>%
  tidyr::gather(`ratio`, `baseline`, key = "type", value = "value")

predict.date <- dmy_hm("01.04.2015 0:30", tz = "Europe/Moscow")
day_raw_plot(predict.date+weeks(3)-days(9), data = test %>% mutate(value = ratio))


gp <- ggplot(data = test.df, aes(x = timestamp, y = value, color = type)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_point(size = 4, fill = "white", shape = 21, na.rm = TRUE) +    # White fill
  geom_line(size = 1, na.rm = TRUE) +
  # scale_color_manual(values = wes_palette("Moonrise2")) +
  # ������ � 3 ���� ��� ������ �� �������� �������
  # http://stackoverflow.com/questions/10339618/what-is-the-appropriate-timezone-argument-syntax-for-scale-datetime-in-ggplot
  #    scale_x_datetime(labels = date_format_tz("%H:%M", tz="Europe/Moscow"), breaks = date_breaks("1 hours"), minor_breaks = date_breaks("30 mins")) +
  scale_x_datetime(labels = date_format_tz("%d-%m-%Y %H:%M", tz="Europe/Moscow"), breaks = date_breaks("4 hours"), minor_breaks = date_breaks("30 mins")) +
  labs(x="����", y="Interface load, %")
# %d.%m (%a)  
gp

stop()



if(FALSE){
  # --- ������������� ������ �� ������������, ��������� �������� ������)
  subdata <- dplyr::sample_frac(subdata, 0.2, replace = FALSE) %>%
    arrange(timestamp) # replace = TRUE -- ��������� ������ ������������� �������. = FALSE -- ���
  # ���������, ���� �� ������������� ������
  subdata %>% group_by(timestamp) %>% filter(n()>1) %>% summarize(n=n())
}

#==================  �������� � ������������� �����������, ����� ��������� =============

# ������� ������� 'integr', value' � 'ratio' � �������� �� � ����� ����� ��������
# �������� ������, ���� ����, ���� �����
subdata %<>% mutate(ratio = value / mean_value)

# ����� ��������� ������� 'subdata.f(iltered)', ����������� ���������� �� ��������� ����
# ����� ���������� �� ���� ������. 1 - �����������, 7 - �������
days.stat <- subdata %>%
  group_by(nwday) %>%
  summarise(mean_integr = mean(integr), sd_integr = sd(integr)) %>%
  arrange(nwday)

subdata.f <-
  subdata %>%
  dplyr::filter(integr >= days.stat$mean_integr[nwday] - 1.9 * days.stat$sd_integr[nwday] &
                  integr <= days.stat$mean_integr[nwday] + 1.9 * days.stat$sd_integr[nwday])

# ����� ���� ��� �� ��������������!!
subdata <- subdata.f

# *********************** ���������� �������� ******************************
print("Start Math")
# Start the clock!
ptm <- proc.time()

# ======================================================================
# ������ ��� �������� � �������, ���� ���������, ��� ��������� � �������� ���������� ������� (�����)
# ���� ����������. C������, ��� � �������� �������� �������� ����� ���� �������� �� �����.
# ����������� �� ��������� ����� ���� � ��������, � ����� ���� ������� � � ����������� ���������.

# ��� �������� ��������� �� ��������� ��������� (����\���\���� ����) 
# ������ ������� � ������ ������ y=f(time)

# ������� �������-�������(3-5) ��� ������� ������������������ (������ ������)
# � ������ ����, ��� �������� ���������� �� 0 �� ��������, ������������� ��������� ������ �������� ���. 
# �� ���� �����, ��� ��� ����� ������� ���������.

flog.info("Regression analysis started")

regr_hdata <- NULL 
days.fit <- NULL # ������ ���������� ������������� ������

for (prognosis.nwday in 1:7) # ���� ������ �� ������� �� ����� ����������� �������. 1 - ����, 7 - �������
{
  # transform(nwday=ifelse(nwday==3 | nwday==5, 4, nwday)) %>%
  # dplyr::mutate() works the same way as plyr::mutate() and similarly to
  # base::transform(). The key difference between mutate() and transform() is
  # that mutate allows you to refer to columns that you just created:
  # https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
  
  # ������� �������-������� ��� ������� ������������������ (������ ������)
  df8 <- subdata %>%
    # mutate(nwday=ifelse(nwday==3 | nwday==5, 4, nwday)) %>%
    filter(nwday == prognosis.nwday) %>%
    arrange(date)
  
  
  df7 <- df8 %>%
    group_by(date, textdate, integr) %>%
    summarise(mean = mean(mean_value), std.dev = sd(mean_value)) %>%  # � ����� ������ mean(value) !!!
    arrange(date)
  
  # ��� ����� ������� ����� �� ��������
  # ����������, � ���������� �� ������ ��� ���������� ���������?
  r.depth <- dim(df7)[1]
  flog.info(str_c("���� ������ - ", wdnames[prognosis.nwday], 
                  ". ���������� ��� ��� �������� ��������� - ",  r.depth))
  
  # �������� ����������
  # http://stackoverflow.com/questions/21677923/how-to-remove-selected-r-variables-without-having-to-type-their-names
  # ������ � ��������� ����������
  # http://stackoverflow.com/questions/17218404/shoud-i-get-a-habit-of-removing-unused-variables-in-r
  
  # �������, ��� ��� ��������� �������� ����������������� � ��������� ������������������ (����������)
  # boxplot ��������� ��������� ��� ��������� ����� ������� �� ��������� � �� ��������.
  # ��� ��������� ���������� ������������ ����������� ���������� x
  # ������, ��������� �� ����������� ����� ���� ���������, �� ����� ����� ���������� ����������� k
  
  # ******************** ��������� �������� ��������� **********************
  
  # ��������� ��������� �������� ���������
  # http://www.tatvic.com/blog/linear-regression-using-r/
  # http://www.theanalysisfactor.com/r-tutorial-4/
  # http://blog.yhathq.com/posts/r-lm-summary.html
  nwday_fit <- lm(data = df7, formula = mean ~ date)
  # summary(days.fit)
  # � ������ ������� ������� � ���� ������ � ������
  days.fit <- rbind(days.fit,
                    data_frame(
                      nwday = as.character(prognosis.nwday),
                      intercept = as.numeric(coef(nwday_fit)[1]),
                      slope = as.numeric(coef(nwday_fit)[2]),
                      r2 = as.numeric(summary(nwday_fit)$r.squared)
                    ))
  
  
  # ******************** ��������� �������� ��������� **********************
  # ��������� ��������� ��� ������� ����
  # ���������� do(): https://cran.r-project.org/web/packages/dplyr/README.html
  # http://stackoverflow.com/questions/22182442/dplyr-how-to-apply-do-on-result-of-group-by
  regr_day <- select(df8, nwday, hgroup, date, ratio) %>%
    group_by(hgroup) %>%
    #do(text = tfun(.))
    do(lm_eqn(.)) %>%
    filter(!is.na(slope)) # ������ ��� ������� ��� ��� ������������� ����������
  
  # � ������ ������� ������� � ���� ������ � ������
  regr_day$nwday <- prognosis.nwday
  #req <- plyr::ddply(df8, .(hgroup), lm_eqn)
  
  # dplyr::bind_rows(regr_hdata, regr_day) # �������� ���������� �� ��� ������ ���
  # � ��� ��� ����� warning, ������ 7 ����: In rbind_all(out[[1]]) : Unequal factor levels: coercing to character
  regr_hdata <- rbind(regr_hdata, regr_day) # https://stat.ethz.ch/pipermail/r-help/2006-June/107734.html
}

flog.info("Regression analysis finished")

# Stop the clock
proc.time() - ptm
print("End Math")
# *********************** ���������� ����������� ******************************

# ������� �������-������� ��� ������� ������������������ (������ ������)
# ���������� ������ ����������� �������� � ��������� �� ����
##### plot_mean_integr()

# ******************** ��������� ��������� ������������� � ��������� **********************
# ����������� ������ ������ ���� �� �����, �����, 24 �������. ��� X - �����, ��� Y - ���������� ��������
##### plot_regr_hdata()

# ===============================================================================
# ������������ ������� ����� ������� �����

reconstruct.point <- function (predict.date) {
  # � �������� �������� ��������� ����������� ���� � ����� �� ������� ���������� �������� ������
  # �� ������ �������� ������� ��������
  
  # ������������� �� ���� ������ � ��� ��� ������ �����.
  # ���������, ��� ���� ������ �����. ����� ������������
  # http://stackoverflow.com/questions/1169248/r-function-for-testing-if-a-vector-contains-a-given-element
  # wday(predict.date) %in% c(3:5)
  # any(wday(predict.date)== c(3:5))
  # print(wday(predict.date))
  # print(">")
  
  nwday <- wday(predict.date)
  # ��-���
  # ��� ������������� �������� �� ����, � �������� ���� ������� ��������� �������� ���
  # browser()
  #p.date <- floor_date(predict.date, "day") + hours(12) # ��������� ���� � �������� 12 �����
  p.date <- floor_date(predict.date, "day")
  # p.hour <- hour(predict.date) # ���� ������� �� ���������
  p.hour.l <- hgroup.enum(predict.date)
  p.hour.r <- hgroup.enum(predict.date + minutes(15)) # ������ ������� ������� �� ���� ��� ����� ������
  
  
  ## paste("����", p.date, ", ����� �������", p.hour.l, ", ������ �������", p.hour.r)
  
  # �������� ���������� �������� ��������� ��������� ������� �� �������� ��� ���������� ���
  # ���� ���� ��������
  day_fit <- days.fit[days.fit$nwday == nwday, ]
  av.load <- as.numeric(day_fit$slope) * as.numeric(p.date) + as.numeric(day_fit$intercept)
  
  # �������� ������ ���� ���������� ����������� ��������.
  # ����������� ����� �������
  # �� &&, � &
  regr.row <- regr_hdata[regr_hdata$hgroup == p.hour.l & regr_hdata$nwday == nwday, ] # �� ��������, ��� ������������� ������ ������������� �� ������� ������� + ���������� ���� � 1
  # regr_hdata[p.hour + 1, ] # �� ��������, ��� ������������� ������ ������������� �� ������� ������� + ���������� ���� � 1
  modif.l <- regr.row$slope * as.numeric(p.date) + regr.row$intercept
  # ����������� ������ �������
  regr.row <- regr_hdata[regr_hdata$hgroup == p.hour.r & regr_hdata$nwday == nwday, ] # �� ��������, ��� ������������� ������ ������������� �� ������� ������� + ���������� ���� � 1
  # regr_hdata[p.hour + 1, ] # �� ��������, ��� ������������� ������ ������������� �� ������� ������� + ���������� ���� � 1
  modif.r <- regr.row$slope * as.numeric(p.date) + regr.row$intercept
  
  # ������ ��������� �� 15-�� �������� ��������� � ���������
  dm <- floor(lubridate::minute(predict.date) / 15) * 15
  walk <- difftime(predict.date, (floor_date(predict.date, "hour") + minutes(dm)), units = "mins")
  
  modif <- modif.l + (modif.r - modif.l) * (as.numeric(walk) / 15)
  p.load <- av.load * modif
  
  # ���� �� ��������� �
  # ����� ����, �� ��������� ����, ��������� � ���������, ����� ��� 5-��
  # �������� ��������� �������� � ���� ������ ���������. ��-�� ����� ��������� ���������, ���� ������ �������������.
  
  as.numeric(p.load) # ���� �������� NA. �� �� �������� melt. Can't melt data.frames with non-atomic 'measure' columns
}

predict.date <- dmy_hm("01.04.2015 0:30", tz = "Europe/Moscow")

# �������� ������ �� ����� ������ ������, ������� �������������� �������
# subdata %<>% mutate()
# 
# tf <- function(m){
# print(m)
# print("<")
# }
# # 
# df <- data.frame(x = 1, y = 1:10)
# m <- lapply(df$y, tf)

#subdata["baseline"] <-
# http://nicercode.github.io/guides/repeating-things/
# https://cran.r-project.org/doc/manuals/R-intro.html#Lists-and-data-frames

print("Reconstructing baseline")
# !!!! lapply �� �������� ��� ["timestamp"], �� ���� ��� $timestamp
# ���� �������� ��������� ����� dput. � ������ $ �������� ������, � ������ [""] �������� data.frame

# --- ������������� ������ �� ������������
# sampledata <- dplyr::sample_frac(subdata, 0.2, replace = FALSE) #20%
sampledata <- subdata

# system.time(baseline.val <- lapply(sampledata$timestamp, reconstruct.point))
# ��� ����
# ������������      �������       ������ 
#        37.87        14.73        53.18 
# ���������� ~ 60�� �� �����

system.time(baseline.val <- purrr::map(sampledata$timestamp, reconstruct.point))
# ��� �����
# ������������      �������       ������ 
#        37.55        14.92        52.89

sampledata$baseline <- as.numeric(baseline.val)
# ��� ���� �� �������� ������������� as.numeric (���� ����� ������ ��� � ������, melt �������� �����������)
# Can't melt data.frames with non-atomic 'measure' columns

#reconstruct.point(predict.date)
print(wday(predict.date))
day_raw_plot(predict.date+weeks(3)+days(1), data = sampledata)

#----
s_date <- ymd("2015-04-01", tz = "Europe/Moscow")
e_date <- s_date + days(1) #17
testdata <- dplyr::filter(sampledata, s_date < timestamp & timestamp < e_date)

baseline_raw_plot(testdata)

jsonF <- toJSON(testdata, pretty=TRUE)
write(jsonF, file="export.JSON")

stop("Manual end")
# =================================================

mm <- filter(df_baseline, abs((baseline-value)/baseline)>.3 & value > 10)


s_date <- ymd("2015-04-01", tz = "Europe/Moscow")
e_date <- s_date + days(10) #17
testdata <- dplyr::filter(subdata, s_date < timestamp & timestamp < e_date)

gp <- baseline_raw_plot(testdata, 12.3)

# Open a new png device to print the figure out to (or use tiff, pdf, etc).
png(filename = "07. adaptive baseline (history).png", width = 11312, height = 8000, units = 'px')
print(gp) #end of print statement
dev.off() #close the png device to save the figure.

write.table(subdata %>% select(timestamp, value, baseline), file = "adaptive_baseline.csv", sep = ",", col.names = NA, qmethod = "double")

#write.xlsx(x = sample.dataframe, file = "test.excelfile.xlsx",
#           sheetName = "TestSheet", row.names = FALSE)

