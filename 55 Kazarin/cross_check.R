library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(magrittr)
library(purrr)
library(stringi)
library(stringr)
library(tibble)
library(readxl)
library(iterators)
library(foreach)
library(doParallel)
library(zoo)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
library(RColorBrewer)


getExcelColName <- function (n) {
  m <- n - 1
  if_else(m < 26, LETTERS[m + 1], paste0(LETTERS[as.integer(m / 26)], LETTERS[as.integer(m %%26 ) + 1]))
}

loadReportsType1 <- function(path){
  # отчет в разрезе ОИП (отчет №1); 022-2016-03кв; несколько файлов эксель
  
  
}

loadReportsType2 <- function(filename){
  # выгрузка из САП (отчет №2); отчет с детализацией по ОКС; один файл эксель
  
  # адресацию ведем по именам колонок, как в Excel, поэтому ручные названия все дропаем
  # хак по считыванию типов колонок
  col_types <- readxl:::xlsx_col_types(filename)
  col_names <- str_c("col_", getExcelColName(seq_along(col_types)))

  #col_types <- rep("text", length(col_types))
  raw <- read_excel(filename,
                    col_types=col_types,
                    col_names=col_names, 
                    skip = 0) # в первой строке просто написано "Отчетная форма"
  
  # выбираем только те строки в которых есть упоминание про ОИП
  ret <- raw %>%
    # filter(str_detect(oip_full, '\\d{3}-\\d{7}\\.\\d{4}')) %>% # 022-2000791.0250
    filter(str_detect(col_A, '\\d{3}-\\d{7}')) # 022-2000791
  
}

loadReportsType3 <- function(path){
  # отчет в разрезе ОКС (отчет №3); отчет о СС ИПР; несколько файлов эксель
  dir(path, pattern="№\\s*3")
  
  
  
}


rep2 <- loadReportsType2("data-verification/Отчет (отчет №2) 022, 056.xlsx")
rep3 <- loadReportsType3("./data-verification/")

#dir(path="./data-verification/", pattern="*№3*.xl*")
