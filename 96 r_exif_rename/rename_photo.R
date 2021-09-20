library(tidyverse)
library(magrittr)
library(stringi)
library(lubridate)
library(stringi)
library(fs)
library(glue)
library(futile.logger)
library(anytime)
library(tictoc)
library(bench)
library(exifr)
library(tictoc)
# library(data.table)
# library(future)
# library(doFuture)

# input_path <- "S:/Camera  Германия-Австрия  raw" %>%
input_path <- "D:/tmp/1" %>%
  fs::path_real()

# выходную директорию надо предварительно создать руками
# output_path <- "S:/2019_08  Германия-Австрия" %>%
output_path <- "d:/tmp/2" %>%
  fs::path_real()


i_fnames <- input_path %>%
  fs::dir_ls(recurse = TRUE, regexp = "(JPG|jpg|JPEG|jpeg)$") # %>%
  # sample(10)

# df <- read_exif(i_fnames)
raw_df <- read_exif(i_fnames, tags = c("SourceFile", "Directory", "FileName", 
                                       "Model", "DateTimeOriginal")) %>%
  # путь к файлу выводится в base64, исправим, чтобы избежать расхождения на всякий случай
  # mutate(tmp = sub("^base64:(.*)", "\\1", SourceFile)) %>%
  # mutate(i_fname = purrr::map_chr(tmp, ~rawToChar(jsonlite::base64_dec(.)))) %>%
  # имя файла еще есть в FileName
  mutate(tm = anytime::anytime(DateTimeOriginal), 
         i_fname = i_fnames, 
         exif_path = stri_c(Directory, "/", FileName)) %>%
  select(i_fname, exif_path, DateTimeOriginal, model = Model, tm)

# посмотрим для проверки расхождения в именах
filter(raw_df, i_fname != exif_path)
  
# проводим корректировку времени и формируем имена выходных файлов
clean_df <- raw_df %>%
  mutate(timestamp = case_when(
    model == 'iPhone 6' ~ tm,
    model == 'iPhone 11' ~ tm,
    model == 'COOLPIX S9900' ~ tm - lubridate::minutes(56),
    TRUE ~ tm)
  ) %>%
  # mutate_at("i_fname", stri_enc_tonative) %>%
  # mutate_at("i_fname", stri_encode, to = "UTF-8") %>%
  # mutate_at("i_fname", stri_conv, to = "windows-1251") %>%
  # mutate_at("i_fname", stri_encode, to = "windows-1251") %>%
  # mutate_at("i_fname", `Encoding<-`, "UTF-8") %>%
  mutate_at("i_fname", fs::path_real) %>%
  mutate(fname = format(timestamp, format = '%Y-%m-%d %H_%M_%S')) %>%
  # для файлов, которые не имеют тегов ("левые"), скопируем имя файла из входного
  mutate(fname = dplyr::coalesce(fname, fs::path_ext_remove(fs::path_file(i_fname)))) %>%
  # еще нюанс, после переименования могут появиться дубликаты из разных источников
  group_by(fname) %>%
  mutate(n = n(), idx = row_number()) %>%
  ungroup() %>%
  # для неуникальных имен добавим индекс в конце
  mutate(fname = case_when(
    n > 1 ~ stri_c(fname, '#', idx),
    TRUE ~ fname
    )
  ) %>%
  mutate(ext = fs::path_ext(i_fname)) %>%
  mutate(o_fname = fs::path(!!output_path, fs::path_ext_set(fname, ext))) # %>%
  # mutate_at(vars(i_fname, o_fname), stri_enc_tonative)
  
filter(clean_df, is.na(o_fname))
stri_enc_detect2(clean_df$i_fname)
Encoding(clean_df$i_fname)
Encoding(clean_df$o_fname)

# посмотрим дубли на выход
janitor::get_dupes(clean_df, o_fname)

# копируем файлы
tic("Копируем файлы")
clean_df %$%
  # purrr::walk2(i_fname, o_fname, ~print(glue("{.x} -> {.y}")))
  purrr::walk2(i_fname, o_fname, ~fs::file_copy(.x, .y, overwrite = TRUE))
toc()

# можно посчитать число входных и выходных файлов на диске для проверки результата работы

#
# fs::path_file("S:/Camera  Германия-Австрия  raw/iPad/IMG_2250.JPG")

# df$SourceFile[[1]] %>% 
#   sub("^base64:(.*)", "\\1", .) %>%
#   # base64enc::base64decode() %>%
#   jsonlite::base64_dec() %>%
#   rawToChar()
# 
# 
# purrr::map(purrr::safely(writeToClickhouse))
# flog.info("Load finished")