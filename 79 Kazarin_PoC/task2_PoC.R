library(tidyverse)
library(lubridate)
library(readr)
library(readxl)
library(stringi)
library(profvis)
library(anytime)
library(config)
library(tictoc)
library(pryr)
# library(config)
packageVersion("dplyr")


costing_file <- "./data/task2_sample1.xlsx"
getwd()

raw_df <- read_excel(costing_file) %>%
  select(1:19) # отрезали примечания

# Соберем агрегированные имена колонок
# названия колонок размазаны по строкам 2-4. 2-ая -- группирующая, 3-я -- детализирующая, 4-ая -- номер
# Надо их слить и переименовать колонки, причем приоритет имеет строка 3, как уточняющая
# различные виды join не подойдут, поскольку мы хотим оставить все строки, вне зависимости от результата
# сливать по именам опасно, вдруг есть дубли
names_df <- raw_df %>%
  {tibble(name_c2=gather(.[2, ], key=name, value=v)$v,
          name_c3=gather(.[3, ], key=name, value=v)$v,
          name_c4=gather(.[4, ], key=name, value=v)$v)} %>%
  # http://www.markhneedham.com/blog/2015/06/28/r-dplyr-update-rows-with-earlierprevious-rows-values/
  # mutate(name_c2 = na.locf(name_c2)) %>%
  fill(name_c2) %>%
  # если name_c3 = NA, то результат объединения строк также будет NA, нас это не очень устраивает
  # replace_na(list(name_c3="")) %>%
  mutate(complex_name=if_else(is.na(name_c3),
                              stri_join(name_c4, name_c2, sep=": "),
                              stri_join(name_c4, name_c2, name_c3, sep=": ")))

# поименовали колонки
names(raw_df) <- names_df$complex_name

#  [1] "1: №№ п.п."                                         "2: № Объектной, локальной сметы"                   
#  [3] "3: Обозначения: Код вида ОКС"                       "4: Обозначения: Код ОКС"                           
#  [5] "5: Обозначения: Код вида ОССР"                      "6: Обозначения: Код ОССР"                          
#  [7] "7: Обозначения: Номер главы ССР"                    "8: Обозначения: Код вида затрат"                   
#  [9] "9: Наименование СД"                                 "10: Объемные показатели: Ед.изм."                  
# [11] "11: Объемные показатели: кол-во"                    "12: Сметная стоимость, руб.: строительные работы"  
# [13] "13: Сметная стоимость, руб.: монтажные работы"      "14: Сметная стоимость, руб.: оборудование"         
# [15] "15: Сметная стоимость, руб.: прочие работы"         "16: Сметная стоимость, руб.: всего"                
# [17] "17: Машинный номер"                                 "18: Основание(спецификация, чертежи, схемы и т.п.)"
# [19] "19: Шифр тома СД"  


# теперь вручную переименуем ряд колонок, которые будем использовать в анализе и отрежем верхний мусор
df0 <- raw_df %>% 
  rename_at(c(3, 4, 5, 6, 7), 
            funs(c("oks_type", "oks_code", "ossr_type", "ossr_code", "ssr_chap"))) %>%
  # именуем колонки для последующей свертки
  rename_at(12:16, 
           funs(c("строительные работы", "монтажные работы", "оборудование", "прочие работы", "всего"))) %>%
  rename_at(12:16, 
            funs(c("графа 12", "графа 13", "графа 14", "графа 15", "графа 16"))) %>%
  slice(6:n())

# классификация и разбивка --------------------
# Прямые затраты относятся на конкретный ОССР и ОКС, т.е Код ОКС и Код ОССР отличен от "0000", 
# Если Коды ОКС и ОССР = "0000" то эти затраты косвенные, они относятся на проект в целом 
# и их нужно распределять на стоимости каждого ОКС пропорционально доли прямых затрат.

# последовательность действий некоммутативна, поскольку идет адресация по индексам
df1 <- df0 %>%
  # исключим ненужный правый мусор
  select(-(17:19)) %>%
  # определим тип затрат
  # mutate(indirect_cost=if_else(oks_code=="0000" & ossr_code=="000", TRUE, FALSE)) # %>%
  mutate(indirect_cost=(oks_code=="0000" & ossr_code=="000")) %>%
  # свернем все типы затрат, после этого к исходным индексам возвращаться уже нельзя
  gather(12:16, key="estimated_cost_type", value="estimated_cost") %>%
  select(oks_type, oks_code, ossr_code, ssr_chap, indirect_cost, 
         estimated_cost_type, estimated_cost, `9: Наименование СД`) %>%
  filter(complete.cases(.)) %>%
  mutate_at(vars(estimated_cost), as.numeric)


calc_cost <- function(chap, df){
  print(paste0("Глава ", chap))
  # сюда заносим логику вычисления для каждой главы отдельно
  calc_rules <- list("01"=c("графа 12", "графа 13"), "08"=c("графа 12", "графа 13"),
                     "09"=c("графа 12", "графа 13", "графа 14"), "10"=c("графа 16"),
                     "12"=c("графа 16"))
  res <- df %>%
      filter(estimated_cost_type %in% calc_rules[[chap]]) %>%
      summarise(res=sum(estimated_cost)) %>%
      pull(res)

  print(res)    
  res
  }
  
# определим косвенные затраты по главам
ind_df <- df1 %>%
  filter(indirect_cost) %>%
  filter(ssr_chap %in% c("01", "08", "09", "10", "12")) %>%
  arrange(ssr_chap) %>%
  group_by(ssr_chap) %>%
  nest() %>%
  # посчитаем базу распределения в зависимости от Главы ССР
  mutate(chap_indirect_сost=purrr::map2_dbl(ssr_chap, data, ~calc_cost(.x, .y)))
         

object_size(ind_df)

# разнесем косвенные затраты

stop()
# посмотрим на данные -----------------
tic("Analysis")
# посчитаем количество уникальных значений в колонках
dist_cols <- map_df(select(df1, everything()), n_distinct)
# посчитаем и отсортируем словарные уникальные значения
unq <- map(df1, unique) %>% map(sort, decreasing=T)
toc()


dist_cols
unq
distinct(df1) # посмотрим на различные комбинации



stop()


# посмотрим на данные
m <- raw_df[2, ] %>% flatten_chr()

names <- stri_join(raw_df[2, ] %>% replace_na(replace=list()), " : ", raw_df[3, ], ignore_null=TRUE)

# , locale=locale("ru", encoding="windows-1251"), col_types="ccccccccc")
# поскольку импорт автоматический, то принудительно загоним все в строки для последующей ручной обработки
print(problems(raw_df), n=Inf)

# постпроцессинг
df0 <- raw_df %>% 
  # переименуем колонки из требования фиксирвоанных позиций
  setNames(c("billing_code", "account", "equipment_sn", "service_name", 
             "service_code", "charge", "service_type", "payment", "charge_period")) %>%
  # выкидываем строки, в которых были ошибки парсинга
  filter((!row_number() %in% problems(.)$row)) %>%
  # преобразуем числовые поля в числа
  mutate_at(vars(charge, payment), as.numeric) %>%
  mutate_at(vars(charge_period), myd, truncated=1) %>%
  mutate_at(vars(account, equipment_sn), function(x) stri_replace_all_regex(x, "^'", "")) %>%
  mutate(id=row_number())

# логическая отбраковка
df1 <- df0 %>%
  separate_rows(equipment_sn) %>% 
  # отбракуем отсутствие начислений
  filter(!is.na(charge)) %>%
  # ограничение на длину SN
  filter(!is.na(equipment_sn)) %>%
  # filter(stri_length(equipment_sn)==11) %>%
  # номер состоит строго из 11 цифровых знаков и начинаться на 001..009
  filter(!is.na(stri_match_first_regex(equipment_sn, "^00[1-9][:digit:]{8}"))) %>%
  # head(50) %>%
  # размажем платежи
  group_by(id) %>%
  mutate(split_charge=charge/n(), split_payment=payment/n()) %>%
  ungroup() %>%
  select(equipment_sn, everything()) %>%
  arrange(equipment_sn)
  
    

# count_fields(xdr_file, tokenizer_tsv())

stop()

write_csv(df1, "clean_xdr.csv")

df <- df1
