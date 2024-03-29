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

# �������� � ��������� ����� �����, ����������� �������� dir
# ���� ����������, ��� ���� �������������� � unicode �� �����-�� ����

getExcelFileList <- function(path=".", pattern="") {
  basename_matches <- dir(path, pattern, full.names=TRUE)
  basename_matches
  # ���������� ���� ��������� �����
  partial_matches <- stri_match_all_regex(basename_matches, pattern='^.*/(?!\\~)[^/]*?\\.xl.*$') # ��� list
  full_matches <- na.omit(unlist(partial_matches))
  # ��� ���������� ������ ���� ����� �������� path <- readxl:::check_file(path) !!!!
  
  # stri_encode(full_matches, to=stri_enc_get())
  # full_matches <- partial_matches[!sapply(partial_matches, function(x) all(is.na(x)))] # ������� NA
  #str(full_matches)
}

loadExcelReportList <- function(file_list) {
  # ������ ��� ������� 1 � 3
  # browser()
  ret <-
    foreach(fname = iter(file_list),
            .packages = 'futile.logger',
            .combine = rbind) %do% {
              browser()
              
              # ��������� ����� �� ������ �������, ��� � Excel, ������� ������ �������� ��� �������
              # ��� �� ���������� ����� �������
              
              file_format <- readxl:::excel_format(fname)
              raw <- read_excel(fname, col_names=FALSE)
              
              # col_types <- readxl:::xlsx_col_types(fname)
              col_types <- rep("text", ncol(raw))
              col_names <- str_c("grp_", seq_along(col_types))
              
              raw <- read_excel(fname,
                                col_types = col_types,
                                col_names = col_names,
                                skip = 0) # � ������ ������ ������ �������� "�������� �����"
              raw
            }
}

my_path <- "./data/"
# ��� �� �������� -----------------------------------------
# ������: zip file './data/Report №1.xlsx' cannot be opened
file_list <- getExcelFileList(my_path, pattern=".*�\\s*1")
df_list <- map(file_list, read_excel)
raw <- loadExcelReportList(file_list)

# 
# � ��� �������� ------------------------------------------
# ������, ��-�� ����, ��� � �������� ���������� ��� ������ 
# ��������� �������������� � utf
file_list <- dir(my_path, pattern=".*�\\s*1", full.names=TRUE)
df_list <- map(file_list, read_excel)
# �������� ������ ��� utf
partial_matches <- stri_match_all_regex(file_list, pattern='^.*/(?!\\~)[^/]*?\\.xl.*$')
file_list2 <- unlist(partial_matches)
identical(file_list, file_list2)
df_list <- map(file_list2, read_excel) # � ��� ����� ������

file_list3 <- stri_conv(file_list2, from="UTF-8", to="windows-1251", to_raw=FALSE)
file_list3 <- stri_conv(file_list2, from="UTF-8", to_raw=FALSE)
df_list <- map(file_list3, read_excel) # � ��� ������!


# ���������� � ����������� --------------------------------
stri_enc_list()
fname <- file_list2[[2]]
stri_enc_detect2(fname)
m <- stri_conv(fname, from = "UTF-8", to = NULL, to_raw = FALSE)
stri_enc_detect2(m)
