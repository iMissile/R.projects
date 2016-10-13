library(dplyr)
library(lubridate)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)
library(readxl)

t = tibble(n = c(0.3, 0.95, 0.2, 0.58, 0.1, 0.85, 0.95, 0.82)) %>% mutate(new = n*255)

# хак по считыванию типов колонок
col_types <- readxl:::xlsx_col_types("merged_master_families_matrix_ver_0_13.xlsx")
col_types <- rep("text", length(col_types))
mmfm <- read_excel("merged_master_families_matrix_ver_0_13.xlsx", 
                   sheet = "merged_master_families_matrix", 
                   col_types = col_types)
# имеем проблему, колонки с NA вместо имени

# можно писать в одно преобразование, но специально разбил на шаги
# трансформируем колонки
df0 <- mmfm %>%
  repair_names(prefix = "repaired_", sep = "")

# теперь удалим колонки, которые имели кривые имена (NA)
df1 <- df0 %>%
  select(-starts_with("repaired_"))

# с датами тоже проблема, поскольку их пишут от произвольно, либо дата, либо строка '20121231', 
# а excel еще и сам преобразует даты в числа.
# в итоге, числа, которые больше 10^7 означают строку на распознавание, 
# а меньшее -- дата в численном виде excel
# а еще есть какие-то обрывочные строки внизу (куски ДНК)
# использовать na.omit() не получается, из 450 строк остается 86, 
# поэтому удалим те, у которых дата отсутствует
df2 <- df1 %>%
  mutate(numpd = as.numeric(.[["Priority date"]])) %>%
  filter(!is.na(numpd)) # удаляем все строки, содержащие пустые данные

# dput(ymd(20090122)) # дает Date, а не POSIXct! Вообще-то, неплохо понять, что мы хотим иметь на выходе
# вариантов множество, но 

# векторизируем с помощью враппера, чтобы применить внутри mutate
parseDate <- Vectorize(function(x){
 if(x > 1e7) ymd(x) else as.Date(x, origin = "1899-12-30") # origin из мануала as.Date
})

# где-то в ходе преобразований Date теряет свой класс и превращается просто в набор чисел
# принудительно вернем назад
df <- df2 %>%
  mutate(priority_date = as.Date(parseDate(numpd), origin = "1970-01-01"))
  
t <- df %>%
  select(priority_date)

# из мануала as.Date
## Excel is said to use 1900-01-01 as day 1 (Windows default) or
## 1904-01-01 as day 0 (Mac default), but this is complicated by Excel
## incorrectly treating 1900 as a leap year.
## So for dates (post-1901) from Windows Excel
as.Date(35981, origin = "1899-12-30") # 1998-07-05
## and Mac Excel
as.Date(34519, origin = "1904-01-01") # 1998-07-05
## (these values come from http://support.microsoft.com/kb/214330)
  
  
#  mutate(priority_date = if_else(numpd > 1e7, ymd(.[["Priority date"]]),
#                                 as.POSIXct((numpd - 25569) * 86400, tz = "GMT", origin = "1970-01-01")))


stop()

# самый простой вариант
# d <- purrr::map(c(20090122, 20090123, 34890), pd)
# df2$priority_date <- purrr::map(c(20090122, 20090123, 34890), pd)

dput(df$priority_date)  

mutate(priority_date = if_else(numpd > 1e7, 0, numpd))

ymd(numpd) 

mutate(priority_date = if_else(numpd > 1e7, ymd(.[["Priority date"]]),
                               as.Date(numpd, origin = "1899-12-30")))



m <- as.numeric(mmfm[["Priority date"]])

#as.character(as.POSIXct(as.numeric(mmfm[["Priority date"]]) * (60*60*24), origin="1899-12-30", tz="GMT"), format = "%Y%m%d")

#mmfm[nchar(mmfm[["Priority date"]]) == 5, "Priority date"]

mmfm[nchar(mmfm[["Priority date"]]) == 5, "Priority date"] <- 
  as.character(as.POSIXct(as.numeric(mmfm[nchar(mmfm[["Priority date"]]) == 5, "Priority date"]) * (60*60*24)
    , origin="1899-12-30"
    , tz="GMT"), format = "%Y%m%d")

# ==================
# тест
n.df <- tibble(ss = c("dkdld", "dd", "dsdllsse"))

n.df %>%
  filter(nchar(ss) > 3)
