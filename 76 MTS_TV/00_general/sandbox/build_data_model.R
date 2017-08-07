library(tidyverse)
library(lubridate)
library(magrittr)
library(forcats)
library(ggrepel)
library(stringi)
library(stringr)
library(shiny)
library(jsonlite)
#library(DBI)
#library(RPostgreSQL)
library(anytime)
library(tictoc)
library(profvis)
library(microbenchmark)
library(Cairo)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
# library(debug)
library(config)


source("clickhouse.R")




rm(list=ls()) # очистим все переменные
df0 <- jsonlite::fromJSON("datamodel.json", simplifyDataFrame=TRUE)

# построим модель переменных для вычисления
df2 <- df0 %>%
  as_tibble() %>%
  # в переменные берем только то, что подпадает под агрегаты
  filter(!is.na(aggr_ops)) %>%
  separate_rows(aggr_ops) %>%
  mutate(id=row_number()) %>%
  # переводим алиасы функций агрегации в различные узлы (экран, CH)
  mutate(x=aggr_ops,
         ext_aggr_opts=case_when(
           x=="min" ~ "мин, min",
           x=="max" ~ "макс, max",
           x=="mean" ~ "ср, avg",
           x=="sum" ~ "сумма, sum",
           x=="unique" ~ "уник, uniq",           
           x=="count" ~ "всего, count",
           TRUE ~ "UNKNOWN"
         )) %>%
  # разнесем на отдельные колонки
  separate(ext_aggr_opts, into=c("visual_aggr_func", "ch_aggr_func")) %>%
  select(-x) %>%
  mutate(visual_var_name=str_c(col_runame_screen, ": ", visual_aggr_func)) %>%
  mutate(visual_group_name=str_c("gr ", "(", col_name, ")")) %>%
  mutate(ch_field=case_when(
    col_name=="prefix" ~ "substring(serial, 1, 3)",
    TRUE ~ col_name)) %>%
  mutate(ch_query_name=str_c(ch_aggr_func, "(", ch_field, ")"))
  
  

#data <- as.list(df2$query_name)
#names(data) <- df2$visual_name
data_model_df <- df2
data <- setNames(as.list(data_model_df$ch_query_name), data_model_df$visual_var_name)

# построим список переменных в часть SELECT
data_model_df %>%
  {purrr::map2_chr(.$ch_query_name, .$col_name, ~str_c(.x, " AS ", .y))} %>%
  stri_join(sep="", collapse=", ")

# теперь заполним возможные выборы в сооотв. с метамоделью
# updateSelectizeInput(session, 'foo', choices=data, server=TRUE)
stop()

# блок для моделирования и добавления новых полей
df <- jsonlite::fromJSON("datamodel.json", simplifyDataFrame=TRUE)# %>%
  # select(-visual_name, -query_name)
  # mutate(can_be_grouped=FALSE) 
  # rename(visual_var_name=visual_name, visual_group_name=can_be_grouped)

model_json <- jsonlite::toJSON(df, pretty=TRUE)
write_lines(model_json, "datamodel_new.json") # сохраняем в UTF

# write_json(df, "datamodel_new.json") #, sep = "\t", fileEncoding="utf-8")


# df1 <- separate_rows(df, aggr_ops)



