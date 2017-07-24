library(tidyverse)
library(magrittr)
library(officer)
library(stringi)


# проверяем запись таблицы с русскими именами под win

out_df <- tibble(time=c(1, 2), value=c("ns", ":dm"))
bad_df <- tibble("Время"=c(1, 2), value=c("ns", ":dm"))

nm <- names(bad_df)
stri_enc_mark(nm)
stri_enc_mark(stri_conv(nm, to="UTF-8", to_raw=FALSE))
Encoding(nm[[2]]) <- "UTF-8"
stri_enc_mark(nm)

names(bad_df) <- nm

doc <- read_docx() %>% # read_docx(path="./TV_report_template.docx") %>%
  body_add_par(value='Первые 80 строк данных', style="heading 1") %>%
  body_add_table(value=out_df, style="table_template") %>%
  body_add_table(value=bad_df, style="table_template") %>% 
  print(target = "test1.docx")
