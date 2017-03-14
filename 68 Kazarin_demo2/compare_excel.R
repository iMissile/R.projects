library(tidyverse)
library(readxl)
library(magrittr)

# Key - oip, value - ss1 or ss2 or ss3

old_df <- read_excel("./data/old.xlsx")
new_df <- read_excel("./data/new.xlsx")
  
# можно, конечно, интерактивно выбирать файлы функцией file.choose()

key_name <- "oip"
val_name <- "ss2"
values <- list(new=paste0(val_name, "_new"),
               old=paste0(val_name, "_old"))

# делаем сравнение в 2 шага (можно все в рамках одного потока исполнения):
# 1. сливаем файлы по ключу
# Можно поглядеть http://stat545.com/bit001_dplyr-cheatsheet.html
diff_df <- new_df %>% 
  full_join(old_df, by=key_name, suffix=c("_new", "_old")) %>%
  # для упрощения жизни переименуем колонки в фиксированные имена, replyr пока не трогаем
  rename_(new_val=as.name(values$new)) %>%
  rename_(old_val=as.name(values$old)) %>%
# поскольку NA по определению насравнимы (NA is noncomparable by != ), 
# то для упрощения цепочки обработки заменим NA на "левое" значение
  tidyr::replace_na(replace=list(new_val=-9999, old_val=-9999))

# 2. удаляем сроки с идентичными сравниваемыми значениями
diff_df %<>% 
  filter(new_val != old_val) %>%
# 3. оставляем только сравниваемые колонки
  select(oip, new_val, old_val)


stop()

# 2. удаляем сроки с идентичными сравниваемыми значениями
diff_df %<>% 
  # здесь приходится ухищряться, поскольку имена сравниваемых колонок передаем 
  # как переменную из внешнего окружения
  filter_(lazyeval::interp(~ new!=old, 
                           new=as.name(values$new),
                           old=as.name(values$old))) %>%
  # 3. оставляем только сравниваемые колонки
  # теоретически, при остутствии переменной, идет иерархический подъем, поэтому делаем без NSE
  select(oip, one_of(unlist(values)))





# немного размышлений про SE\NSE ======================================================

  
  filter_(lazyeval::interp(~ new==old, 
                           new=as.name(paste0(val_name, "_new")),
                           old=as.name(paste0(val_name, "_old"))))

  
  
  filter_(lazyeval::interp(~ new==old, 
                           new=paste0(as.name("val_name"), "_new"),
                           old=paste0(as.name("val_name"), "_old")))

  filter_(lazyeval::interp(~ new==old, 
                           new=as.name(paste0(val_name, "_new")),
                           old=as.name(paste0(val_name, "_old"))))
  

# ======================================================
  filter_(lazyeval::interp(~ new==old, 
                           .values=list(new=as.name(paste0(val_name, "_new")),
                                        old=as.name(paste0(val_name, "_old")))))
# насчет by=key_name -- работает, но можно подстраховаться через full_join

# опционально можем выбрать колонки, которые интересуют, остальное все выкинуть

# select(one_of(c(key_name, val_name))) %>%
  