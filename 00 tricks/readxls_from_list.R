library(tidyverse)
library(lubridate)
library(magrittr)
library(purrr)
library(stringi)
library(readxl)
library(iterators)
library(foreach)
library(doParallel)
# library(stringr)

# проблема с передачей имени файла, полученного функцией dir
# есть подозрение, что идет преобразование в unicode на каком-то шаге

getExcelFileList <- function(path=".", pattern="") {
  basename_matches <- dir(path, pattern, full.names=TRUE)
  basename_matches
  # пропускаем локи открытого файла
  partial_matches <- stri_match_all_regex(basename_matches, pattern='^.*/(?!\\~)[^/]*?\\.xl.*$') # тут list
  full_matches <- na.omit(unlist(partial_matches))
  # для корректной работы надо потом прогнать path <- readxl:::check_file(path) !!!!
  
  # stri_encode(full_matches, to=stri_enc_get())
  # full_matches <- partial_matches[!sapply(partial_matches, function(x) all(is.na(x)))] # удаляем NA
  #str(full_matches)
}

loadExcelReportList <- function(file_list) {
  # хелпер для отчетов 1 и 3
  # browser()
  ret <-
    foreach(fname = iter(file_list),
            .packages = 'futile.logger',
            .combine = rbind) %do% {
              browser()
              
              # адресацию ведем по именам колонок, как в Excel, поэтому ручные названия все дропаем
              # хак по считыванию типов колонок
              
              file_format <- readxl:::excel_format(fname)
              raw <- read_excel(fname, col_names=FALSE)
              
              # col_types <- readxl:::xlsx_col_types(fname)
              col_types <- rep("text", ncol(raw))
              col_names <- str_c("grp_", seq_along(col_types))
              
              raw <- read_excel(fname,
                                col_types = col_types,
                                col_names = col_names,
                                skip = 0) # в первой строке просто написано "Отчетная форма"
              raw
            }
}

my_path <- "./data/"
# так не работает -----------------------------------------
# ошибка: zip file './data/Report в„–1.xlsx' cannot be opened
file_list <- getExcelFileList(my_path, pattern=".*№\\s*1")
df_list <- map(file_list, read_excel)
raw <- loadExcelReportList(file_list)

# 
# а так работает ------------------------------------------
# видимо, из-за того, что в процессе отсеивания лок файлов 
# произошло преобразование в utf
file_list <- dir(my_path, pattern=".*№\\s*1", full.names=TRUE)
df_list <- map(file_list, read_excel)
# Проверим версию про utf
partial_matches <- stri_match_all_regex(file_list, pattern='^.*/(?!\\~)[^/]*?\\.xl.*$')
file_list2 <- unlist(partial_matches)
identical(file_list, file_list2)
df_list <- map(file_list2, read_excel) # а так опять ошибка

# упражнения с кодировками --------------------------------
m <- file_list2[[2]]
stri_enc_detect2(m)
m <- stri_conv(file_list2[[2]], from = NULL, to = NULL, to_raw = FALSE)