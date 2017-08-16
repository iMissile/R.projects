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



rm(list=ls()) # очистим все переменные
df0 <- jsonlite::fromJSON("data_dict.json", simplifyDataFrame=TRUE)

# на всякий случай защитимся от случая, когда вообще не определено поле internal_name
if (!"internal_name" %in% names(df0)) df0$internal_name <- NA

dict_df <- df0 %>%
  as_tibble() %>%
  # если есть поле в БД, а внутреннее представление не задано, то прозрачно транслируем
  mutate(internal_name={map2_chr(.$db_field, .$internal_name, ~if_else(!is.na(.x) & is.na(.y), .x, .y))})



m <- separate_rows(data_model_df, aggr_ops, sep="[,;[:space:]]+")


  

dict_df <- dplyr::union(var_model_df %>% select(name_enu=internal_name, name_rus=visual_var_name),
                       group_model_df %>% select(name_enu=internal_name, name_rus=visual_group_name))

#data <- as.list(df2$query_name)
#names(data) <- df2$visual_name


stop()

data <- setNames(as.list(var_model_df$internal_name), var_model_df$visual_var_name)

# построим список переменных в часть SELECT
data_model %>%
  {purrr::map2_chr(.$ch_query_name, .$col_name, ~str_c(.x, " AS ", .y))} %>%
  stri_join(sep="", collapse=", ")

# теперь заполним возможные выборы в сооотв. с метамоделью
# updateSelectizeInput(session, 'foo', choices=data, server=TRUE)


# блок для моделирования и добавления новых полей
df <- jsonlite::fromJSON("datamodel.json", simplifyDataFrame=TRUE) %>%
  select(-col_name, -col_runame_office) %>%
  rename(human_name_rus=col_runame_screen) %>%
  arrange(db_field) %>%
  select(db_field, human_name_rus, can_be_grouped, aggr_ops, everything())

df <- jsonlite::fromJSON("datamodel.json", simplifyDataFrame=TRUE) %>%
  mutate(select_string={map2_chr(.$internal_name, .$db_field, 
                                 ~if_else(is.na(.x), .y, stri_join(.y, " AS ", .x)))}) %>%
  mutate(internal_name={map2_chr(.$internal_name, .$db_field, ~if_else(is.na(.x), .y, .x))})
  # select(-visual_name, -query_name)
  # mutate(can_be_grouped=FALSE) 
  # rename(visual_var_name=visual_name, visual_group_name=can_be_grouped)

model_json <- jsonlite::toJSON(df, pretty=TRUE)
write_lines(model_json, "datamodel_new.json") # сохраняем в UTF

# write_json(df, "datamodel_new.json") #, sep = "\t", fileEncoding="utf-8")


# df1 <- separate_rows(df, aggr_ops)

stop()
# первичное создание json словаря
rm(list=ls()) # очистим все переменные
df <- tribble(
  ~col_name, ~col_runame_screen, ~col_runame_office, ~col_label, 
  "region", "регион", "регион","Название региона, указанного в настройках STB",
  "unique_stb", "кол-во уник. STB", "кол-во уник. STB", "Количество уникальных STB в регионе",
  "total_unique_stb", "всего уник. STB", "всего уник. STB", "Общее количество уникальных STB по всем регионам",
  "total_duration", "суммарное время, мин",	"суммарное время, мин",	"Суммарное время просмотра канала всеми STB",
  "watch_events", "кол-во просмотров", "кол-во просмотров", "Суммарное количество событий телесмотрения в регионе",
  "stb_ratio", "% уник. STB", "% уник. STB", "Соотношение STB в регионе к общему количеству STB",
  "segment", "сегмент", "сегмент", "подсказка (segment)",
  "channelId", "канал (ID)", "канал  (ID)", "подсказка (channelId)",
  "channelName", "канал", "канал", "Название телеканала",
  "channel_duration", "суммарное время, мин", "суммарное время, мин", "Суммарное время телесмотрения всеми STB в регионе",
  "mean_duration", "ср. время просмотра, мин", "ср. время просмотра, мин", "Среднее время отдельного просмотра канала",
  "watch_ratio", "% врем. просмотра", "% врем. просмотра", "Отношение времени просмотра канала к общему времени телесмотрения",
  "duration_per_stb", "ср. время просм. 1 STB за период, мин", "ср. время просм. 1 STB за период, мин", "Среднее время суммарного просмотра канала одной приставкой за выбранный период",
  "date", "дата", "дата", "подсказка (date)",
  "timestamp", "время", "время", "подсказка (timestamp)",
  "timegroup", "группа", "группа", "подсказка (timegroup)"
) %>%
  mutate(db_field=col_name) %>%
  select(db_field, internal_name=col_name, human_name_rus=col_runame_screen, 
         -col_runame_office, col_label)
  

dic_json <- jsonlite::toJSON(df, pretty=TRUE)
write_lines(dic_json, "dic_temp.json") # сохраняем в UTF



