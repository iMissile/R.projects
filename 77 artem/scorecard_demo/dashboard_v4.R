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

raw_df <- read_excel(data_file) %>%
  mutate_at(vars(docdate), as_date)

# поглядим сводку по данным ---------------------
if (FALSE) {
  tic()
  # посчитаем количество уникальных значений в колонках
  dist_cols <- raw_df %>% map_df(n_distinct)
  # посчитаем и отсортируем словарные уникальные значения
  unq <- map(raw_df, unique) %>% map(sort, decreasing = T)
  # unq
  dist_cols
  toc()
}

subset_df <- raw_df %>% 
  mutate(deparse=stri_replace_all_regex(name, "(.+\\sТК)\\s+(СКБК|ПЗБМ)\\s?(ОБФ\\s\\d)?\\s?(.*)", 
                                        "$1_$2_$3_$4")) %>%
  separate(deparse, into=c("operation", "group", "material", "kpi"), sep="_") %>%
  # выберем только интересующие операции
  filter(operation %in% c("Выпуск ТК", "Склад ТК", "Отгрузка ТК")) %>%
  # и выкинем весь служебный шлак
  select(-id, -unit, -firmcode, -docnum)

# выбираем данные на конкретную дату
c_date <- dmy("10.09.2015") # current date

# надо будет добавить сюда еще Итого
df0 <- subset_df %>%
  filter(docdate>=c_date-days(2) & docdate<=c_date) %>%
  filter(material!="" & kpi=="")

# надо будет сформировать и добавить сюда еще "Итого"
df1 <- df0 %>%
  group_by(docdate, kpi) %>%
  summarise(actualvalue=sum(actualvalue), planvalue=sum(planvalue), material="Итого") %>%
  ungroup()

score_df <- bind_rows(df0, df1) %>%
  # filter(docdate>=c_date-days(2) & docdate<=c_date) %>%
  # filter(material!="" & kpi=="") %>%
  mutate(status=as.factor(if_else(is.na(planvalue), TRUE, actualvalue>planvalue))) %>%
  mutate(label=format(actualvalue, big.mark=" "))

ggplot(score_df, aes(x=docdate, y=actualvalue)) +
  geom_bar(aes(fill=status), stat="identity") +
  scale_fill_manual(
    values=c("FALSE"="brown1", "TRUE"="chartreuse4"),
    # breaks=c("4", "6", "8"),
    # ручное управление, сортировка по алфафиту
    labels=c("просадка", "в плане")
  ) +
  # нарисуем плановое значение точкой
  geom_point(aes(y=planvalue), colour="blue", shape=16, size=3) +
  geom_label(aes(label=label), position = position_stack(vjust = 0.5), 
             fill="white", colour="black", fontface="bold", hjust=.5) +
  facet_wrap(~material, nrow=1)


