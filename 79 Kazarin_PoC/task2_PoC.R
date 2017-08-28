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
library(wrapr)
# library(config)
packageVersion("dplyr")

# загрузка и первичная очистка исходных данных -------------------
costing_file <- "./data/task2_clean_sample.xlsx"
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
  rename_at(12:17, funs(c("12", "13", "14", "15", "16", "internal_id"))) %>%
  rename_at(8, funs(c("cost_element"))) %>%  
  slice(6:n())

# классификация и разбивка --------------------
# Прямые затраты относятся на конкретный ОССР и ОКС, т.е Код ОКС и Код ОССР отличен от "0000", 
# Если Коды ОКС и ОССР = "0000" то эти затраты косвенные, они относятся на проект в целом 
# и их нужно распределять на стоимости каждого ОКС пропорционально доли прямых затрат.

# последовательность действий некоммутативна, поскольку идет адресация по индексам
clean_df <- df0 %>%
  # исключим ненужный правый мусор
  # колонка "всего (графа 16)" является вычисляемой, поэтому ее исключим
  # появились нюансы с машинным номером (internal_id), пока ничего выкидывать не будем
  # select(-(16:19)) %>%
  # определим тип затрат
  # mutate(indirect_cost=if_else(oks_code=="0000" & ossr_code=="000", TRUE, FALSE)) # %>%
  mutate(indirect_cost=(oks_code=="0000" & ossr_code=="000")) %>%
  # свернем все типы затрат, после этого к исходным индексам возвращаться уже нельзя
  gather(12:15, key="est_cost_entry", value="est_cost") %>%
  select(oks_type, oks_code, ossr_code, ssr_chap, indirect_cost, 
         est_cost_entry, est_cost, `9: Наименование СД`, cost_element, internal_id) %>%
  # элементы без кода вида затрат (cost_element) являются промежуточными подытогами, их тоже надо исключать
  # но сразу исключать нельзя, надо сначала выделить косвенные затраты
  # filter(complete.cases(.)) %>% 
  mutate_at(vars(est_cost), as.numeric) %>%
  mutate_at(vars(est_cost_entry), as.integer) %>%
  # исключим нулевые затраты и промежуточные подытоги
  # filter_at(vars(est_cost, cost_element), any_vars(is.na(.))) %>%
  filter(est_cost != 0) %>%
  # отбрасываем пустые виды затрат и пустой машинный номер
  filter_at(vars(cost_element, internal_id), all_vars(!is.na(.)))


# посчитаем прямые затраты по графам и по Главам ---------
direct_df <- clean_df %>%
  filter(!indirect_cost) %>%
  filter((ssr_chap %in% stri_split_fixed("02,03,04,05,06,07", ",", simplify=TRUE))) %>%
  group_by(ssr_chap, est_cost_entry) %>%
  summarise(total_cost=sum(est_cost)) %>%
  ungroup() %>%
  # mutate_at(vars(est_cost_entry), as.character)#  %>%
  spread(key=est_cost_entry, value=total_cost)

# расчитаем косвенные затраты по Главам --------
calc_cost <- function(chap, df){
  print(paste0("Глава ", chap))
  # сюда заносим логику вычисления для каждой главы отдельно
  calc_rules <- list("01"=12:15, "08"=12:15,
                     "09"=12:15, "10"=12:15, "12"=12:15)
  res <- df %>%
      filter(est_cost_entry %in% calc_rules[[chap]]) %>%
      summarise(res=sum(est_cost)) %>%
      pull(res)
  print(res)    
  res
}
  
indirect_df <- clean_df %>%
  filter(indirect_cost) %>%
  # filter(!is.na(cost_element)) %>% # уже исключили выше
  filter((ssr_chap %in% c("01", "08", "09", "10", "12"))) %>%
  arrange(ssr_chap) %>%
  group_by(ssr_chap) %>%
  nest() %>%
  # посчитаем базу распределения в зависимости от Главы ССР
  mutate(chap_indirect_сost=purrr::map2_dbl(ssr_chap, data, ~calc_cost(.x, .y))) %>%
  select(-data)

# распределяем косвенные затраты по базису в зависимости от Главы ССР -------------
final_df <- clean_df %>%
  filter(!indirect_cost) %>%
  filter((ssr_chap %in% stri_split_fixed("02,03,04,05,06,07", ",", simplify=TRUE))) %>%
  select(-indirect_cost, -cost_element, -internal_id)

# коэффициенты распределения по Главам 1, 8, 9
t1 <- final_df %>% 
  group_by(ssr_chap) %>%
  filter(est_cost_entry %in% c(12, 13)) %>%
  summarise(s=sum(est_cost)) %>%
  # считаем пропорции
  mutate(`01`=s/sum(s), `08`=`01`, `09`=`01`) %>%
  select(-s)

# коэффициенты распределения по Главам 10, 12
t2 <- final_df %>% 
  group_by(ssr_chap) %>%
  filter(est_cost_entry %in% c(12, 13, 14, 15)) %>%
  summarise(s=sum(est_cost)) %>%
  # считаем пропорции
  mutate(`10`=s/sum(s), `12`=`10`) %>%
  select(-s)

tr <- left_join(t1, t2, by="ssr_chap") %>% 
  gather(`01`,`08`,`09`, `10`, `12`, key=indirect_chap, value=ratio) %>%
  # подсоединим косвенные затраты по Главе
  left_join(indirect_df, by=c("indirect_chap"="ssr_chap")) %>%
  mutate(cost_raise=ratio*chap_indirect_сost) #



%>%
  spread(indirect_chap, cost_raise)


# можно делать и напрямик
# Различные базы распределения для Глав 2-7
s1213 <- sum(final_df %>% filter(est_cost_entry %in% c(12, 13)) %>% pull(est_cost))
s1215 <- sum(final_df %>% filter(est_cost_entry %in% c(12, 13, 14, 15)) %>% pull(est_cost))

final_df %<>% group_by(ssr_chap) %>%
  mutate(ch1=sum(if_else(est_cost_entry %in% c(12, 13), est_cost, 0))/s1213) %>%
  mutate(ch8=sum(if_else(est_cost_entry %in% c(12, 13), est_cost, 0))/s1213) %>%
  mutate(ch9=sum(if_else(est_cost_entry %in% c(12, 13), est_cost, 0))/s1213) %>%
  mutate(ch10=sum(if_else(est_cost_entry %in% c(12, 13, 14, 15), est_cost, 0))/s1215) %>%
  mutate(ch12=sum(if_else(est_cost_entry %in% c(12, 13, 14, 15), est_cost, 0))/s1215) %>%
  ungroup()



# промежуточный анализ полученных данных
# 1. посмотрим строки, которые содержат NA
# m1 <- clean_df %>% filter_all(any_vars(is.na(.)))
# m2 <- clean_df %>% filter_at(vars(est_cost, cost_element), any_vars(is.na(.)))
total_direct_cost <- clean_df %>% 
  filter(!indirect_cost) %>% 
  summarise(s=sum(est_cost)) %>% 
  pull(s)

total_indirect_cost <- sum(indirect_df$chap_indirect_сost)
# !!! суммы не бьются
print(sprintf("%.2f (прямые) + %.2f (косвенные) = %.2f. Прямая сумма по таблице = %.2f", 
              total_direct_cost, total_indirect_cost, (total_direct_cost + total_indirect_cost),
              sum(clean_df$est_cost))) 

# поищем, что именно не так, видимо в косвенных затратах остались другие главы
ss <- clean_df %>% 
  filter(indirect_cost) %>%
  filter(!(ssr_chap %in% c("01", "08", "09", "10", "12"))) %>%
  arrange(ssr_chap)
  

# распределим косвенные затраты по главам 2-7 --------
# для этого определим коэффициент косвенных затрат и на него увеличим все основные
raise_coeff <- 1+sum(indirect_df$chap_indirect_сost)/total_direct_cost

final_df <- clean_df %>%
  mutate(should_raise=ssr_chap %in% c("02","03","04","05","06","07")) %>%
  mutate(raised_cost=est_cost*should_raise*raise_coeff)

object_size(final_df)

# сумма по исходному "Всего"
clean_df %>% 
  filter(est_cost_entry %in% c("графа 16")) %>%
  summarise(raw=sum(est_cost))

# сумма по распределениям
final_df %>% 
  summarise(raw=sum(raised_cost))


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


stop()

write_csv(df1, "clean_xdr.csv")

df <- df1

