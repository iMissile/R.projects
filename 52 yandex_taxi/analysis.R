# library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
library(readxl) # https://github.com/hadley/readxl
library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
#library(lubridate) #load second!
#library(scales)
#library(forecast)
#library(stringr)
#library(RColorBrewer)
#library(wesanderson) # https://github.com/karthik/wesanderson
#library(broom)

# readxl - http://poldham.github.io/reading-writing-excel-files-R/

rawdata <- read_excel("./src/tv_sample.xlsx", sheet = 1, col_names = TRUE, col_types =)

# рушится на данных билайна
# read_csv crashes R session on particular .csv file #175
problems(rawdata)

stop()
df <- dplyr::rename(rawdata, y = X63) # %>% filter(is.na(y))
dplyr:::changes(df, rawdata)

# задачи
# 1. выбор колонок, которые имеют тип character. При этом объект может быть не data.frame а tbl_df
# 2. преобразование строки в шестнадцатиричном виде в число
# 3. вывод количества уникальных данных по каждому столбцу

# How do I get the classes of all columns in a data frame?
# http://stackoverflow.com/questions/10661159/how-do-i-get-the-classes-of-all-columns-in-a-data-frame