library(tidyverse)
library(lubridate)
library(forcats)
library(readr)
library(readxl)
library(stringi)
library(profvis)
library(anytime)
library(config)
library(tictoc)
library(pryr)
library(hrbrthemes)
library(ggthemes)
library(wrapr)
library(openxlsx)
# library(config)
packageVersion("dplyr")

# загрузка и первичная очистка исходных данных -------------------
data_file <- "./data/KPI_example.xls"
getwd()

raw_df <- read_excel(data_file)

# поглядим сводку по данным ---------------------
if (TRUE) {
  tic()
  # посчитаем количество уникальных значений в колонках
  dist_cols <- raw_df %>% map_df(n_distinct)
  # посчитаем и отсортируем словарные уникальные значения
  unq <- map(raw_df, unique) %>% map(sort, decreasing = T)
  # unq
  dist_cols
  toc()
}

# выделим только те данные, которые нам интересны для scoreboard

stop()

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
         est_cost_entry, est_cost, "9: Наименование СД", cost_element, internal_id) %>%
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

# промежуточные упражнения с группировками
if(FALSE){
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
  mutate(cost_raise=ratio*chap_indirect_сost) %>%
  arrange(ssr_chap)# %>%
  # spread(indirect_chap, cost_raise)
}

# Делаем в "лоб"
# Различные базы распределения для Глав 2-7

base_cost <- list(s1213=c(12, 13), s1215=c(12, 13, 14, 15)) %>%
  map(~sum(final_df %>% filter(est_cost_entry %in% .x) %>% pull(est_cost)))
s1213 <- base_cost[["s1213"]]
s1215 <- base_cost[["s1215"]]

v <- indirect_df$chap_indirect_сost %>% set_names(indirect_df$ssr_chap)

final_df %<>% group_by(ssr_chap) %>%
  mutate(ch1=if_else(est_cost_entry %in% c(12, 13), est_cost, 0)/s1213 * v[["01"]]) %>%
  mutate(ch8=if_else(est_cost_entry %in% c(12, 13), est_cost, 0)/s1213 * v[["08"]]) %>%
  mutate(ch9=if_else(est_cost_entry %in% c(12, 13), est_cost, 0)/s1213 * v[["09"]]) %>%
  mutate(ch10=if_else(est_cost_entry %in% c(12, 13, 14, 15), est_cost, 0)/s1215 * v[["10"]]) %>%
  mutate(ch12=if_else(est_cost_entry %in% c(12, 13, 14, 15), est_cost, 0)/s1215 * v[["12"]]) %>%
  mutate(overcost=round(ch1+ch8+ch9+ch10+ch12, 2)) %>%
  select(-ch1, -ch8, -ch9,-ch10, -ch12) %>%
  ungroup()

# проверили
# final_df %>% summarise_at(c("ch1", "ch8", "ch9", "ch10", "ch12"), sum)
# final_df %>% summarise_at(vars(ch1, ch8, ch9, ch10, ch12), sum)

write_csv(final_df, "task2.txt")

# посчитаем слайд 2, "Полная сметная стоимость в разрезе объектов ССР"
df <- final_df %>%
  group_by(oks_code, ossr_code) %>%
  mutate(direct_cost=sum(est_cost), indirect_cost=sum(overcost))

# 
oks_dict <- tibble(oks_code=c("0001", "0002"), oks_name=c("ДКС.2В", "УКПГ.2В"))

df <- final_df %>%
  group_by(oks_code) %>%
  summarise(direct_cost=sum(est_cost), indirect_cost=sum(overcost)) %>%
  left_join(oks_dict)

# нарисуем график с разбивкой по классам ОССР
df0 <- final_df %>%
  filter(oks_code=="0002") %>%
  group_by(ossr_code) %>%
  summarise(direct_cost=sum(est_cost), indirect_cost=sum(overcost)) %>%
  mutate(total_cost=direct_cost+indirect_cost) %>%
  # свернем типы затрат
  gather(indirect_cost, direct_cost, key=type, value=cost) %>%
  mutate(label=format(cost, big.mark=" "))
  
# brewer.pal(n=9, name="Greens")[4]
gp <- ggplot(df0, aes(fct_reorder(as.factor(ossr_code), total_cost, .desc=FALSE), cost)) +
  scale_fill_brewer(palette="Dark2") +
  geom_bar(aes(fill=type), alpha=0.5, stat="identity", position="stack") +
  # geom_text(aes(label=label), hjust=+1.1, colour="blue") + # для вертикальных
  # geom_label(aes(label=label), fill="white", colour="black", fontface="bold", hjust=+1.1) +
  geom_label(aes(label=label), position = position_stack(vjust = 0.5), 
             fill="white", colour="black", fontface="bold", hjust=.5) +
  # geom_text_repel(aes(label=label), fontface = 'bold', color = 'blue', nudge_y=0) +
  # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelName) +
  theme_ipsum_rc(base_size=20,
                 subtitle_size=14,
                 axis_title_size=18) +
  theme_solarized(light=FALSE) +
  # theme(axis.text.x = element_text(angle=90)) +
  ylab("Затраты, руб") +
  xlab("Класс ОССР") +
  ggtitle("Структура затрат", subtitle="В разрезе классов ОССР")
  # coord_flip() 

gp

stop()
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

