library(tidyverse)
library(magrittr)
library(stringi)
library(fs)
library(glue)
library(futile.logger)
library(anytime)
library(tictoc)
library(bench)
library(exifr)
# library(data.table)
# library(future)
# library(doFuture)

input_path <- "S:/Camera  Германия-Австрия  raw" %>%
  fs::path_real()

i_fnames <- input_path %>%
  fs::dir_ls(recurse = TRUE, regexp = "(JPG|jpg)$") %>%
  sample(10)

# df <- read_exif(i_fnames)
df <- read_exif(i_fnames, tags = c("SourceFile", "Model", "DateTimeOriginal")) %>%
  # путь к файлу выводится в base64, исправим, чтобы избежать расхождения на всякий случай
  mutate(tmp = sub("^base64:(.*)", "\\1", SourceFile)) %>%
  mutate(i_fname = purrr::map_chr(tmp, ~rawToChar(jsonlite::base64_dec(.)))) %>%
  mutate(timestamp = anytime::anytime(DateTimeOriginal)) %>%
  select(i_fname, DateTimeOriginal, model = Model, timestamp)
  

df$SourceFile[[1]] %>% 
  sub("^base64:(.*)", "\\1", .) %>%
  # base64enc::base64decode() %>%
  jsonlite::base64_dec() %>%
  rawToChar()


purrr::map(purrr::safely(writeToClickhouse))
flog.info("Load finished")