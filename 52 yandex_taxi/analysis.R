library(readr) #Hadley Wickham, http://blog.rstudio.org/2015/04/09/readr-0-1-0/
# library(readxl) # https://github.com/hadley/readxl
library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
#library(scales)
#library(forecast)
library(stringr)
library(purrr)
library(tibble)
#library(RColorBrewer)
#library(wesanderson) # https://github.com/karthik/wesanderson
#library(broom)

# readxl - http://poldham.github.io/reading-writing-excel-files-R/
# rawdata <- read_excel("./src/tv_sample.xlsx", sheet = 1, col_names = TRUE, col_types =)

# определяем timezone
# http://stackoverflow.com/questions/36300381/dplyr-mutate-with-function-call-returning-incorrect-value
# http://unicode.org/repos/cldr/trunk/common/supplemental/windowsZones.xml
get_tz <- function(m) {
  case_when(m == 'Москва' ~ 'Europe/Moscow',
            m == 'Пермь' ~ 'Asia/Yekaterinburg')
}

# http://barryrowlingson.github.io/hadleyverse/#5
# сразу преобразуем время
raw_tv.df <- read_delim(
  './data/tv.csv', delim = ";", quote = "\"",
  col_names = TRUE,
  col_types = str_c(rep('c', 18), collapse = ''),
  locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
  progress = interactive()
)

t <- raw_tv.df %>%
  mutate(timestamp = dmy_hms(str_c(local_date, local_time, sep = ' '), truncated = 3, tz = get_tz(city)))

# сложность в том, что dmy не принимает tz в виде вектора

stop()

raw_installs.df <- read_delim(
  './data/installs.csv', delim = ";", quote = "\"",
  col_types = 'ccc',
  locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
  progress = interactive()
)

raw_visits.df <- read_delim(
  './data/visits.csv', delim = ";", quote = "\"",
  col_types = 'ccccc',
  locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"), # таймзону, в принципе, можно установить здесь
  progress = interactive()
)

# немного промежуточного анализа

problems(raw_tv.df)
problems(raw_installs.df)
problems(raw_visits.df)
unique(raw_tv.df$city)
dplyr::distinct(raw_tv.df, city)

# Хочу получить список уникальных значений по каждой колонке, ознакомиться
t1 <- map(raw_tv.df, unique) # or length
t2 <- map(raw_installs.df, unique)
t3 <- map(raw_visits.df, unique)


# преобразуем время



stop()
df <- dplyr::rename(rawdata, y = X63) # %>% filter(is.na(y))
dplyr:::changes(df, rawdata)

# задачи
# 1. выбор колонок, которые имеют тип character. При этом объект может быть не data.frame а tbl_df
# 2. преобразование строки в шестнадцатиричном виде в число
# 3. вывод количества уникальных данных по каждому столбцу

# How do I get the classes of all columns in a data frame?
# http://stackoverflow.com/questions/10661159/how-do-i-get-the-classes-of-all-columns-in-a-data-frame