library(dplyr)
library(lubridate)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)
library(readxl)
library(stringi)
library(base64enc)


datafile <- "D:/Temp/alcall_details_bristol_w31_2.xls"
# хак по считыванию типов колонок
excel_sheets(datafile)
raw <- read_excel(datafile)

s <- raw$grpname[[2]] # "ITG\\<d1><e8><e3><e0><f0><e5><f2><fb>\\<d1><e8><e3><e0><f0><e5><f2><fb>"
iconv(s, to = "UTF8", from = "windows-1251") # !!! заработало!!!

grp <- iconv(raw$grpname, to = "UTF8", from = "windows-1251") #!!! заработало!!!
  
# s <- enc2utf8(s)
s <- enc2native(s)
m <- "ITG\\\xd1\xe8\xe3\xe0\xf0\xe5\xf2\xfb\\\xd1\xe8\xe3\xe0\xf0\xe5\xf2\xfb"
# Ёто означает "ITG\—игареты\—игареты"  
res <- stringr::str_replace_all(s, "\\\\", "-\x5ca  \x78\  \x78\\x5C--f-"); dput(res) #x5C\x78
s2 <- stri_unescape_unicode(s)

s2 <- gsub("\\\\", "\\x5C", s, fixed = TRUE); cat(s2)
s2 <- gsub("<", "\\x", s2, fixed = TRUE); cat(s2)
s2 <- gsub(">", "", s2, fixed = TRUE); cat(s2)
s3 <- enc2utf8(stri_unescape_unicode(s2)); cat(s3)

iconv(s2, from = "UTF8", to = "windows-1251")


col_types <- readxl:::xlsx_col_types(datafile)

#col_types <- rep("text", length(col_types))
raw <- read_excel(datafile,
                  sheet = "январь",
                  col_types = col_types) #, skip = 1)

