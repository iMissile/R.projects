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


rm(list=ls()) # ������� ��� ����������
df0 <- jsonlite::fromJSON("datamodel.json", simplifyDataFrame=TRUE)

data_model_df <- df0 %>%
  as_tibble() # %>%
  # ������� ���������� ������������� (������ ������������� �� ����� ���� ����������)
  # mutate(internal_name=ifelse(any(names(.) %in% 'internal_name'), internal_name, as.character(NA))) %>%
  # mutate(select_string={map2_chr(.$internal_name, .$db_field, 
  #                                ~if_else(is.na(.x), .y, stri_join(.y, " AS ", .x)))}) %>%
  # mutate(internal_name={map2_chr(.$internal_name, .$db_field, ~if_else(is.na(.x), .y, .x))})  

# %>%
#   mutate(db_field=case_when(
#     col_name=="prefix" ~ "substring(serial, 1, 3)",
#     TRUE ~ col_name))
  
m <- separate_rows(data_model_df, aggr_ops, sep="[,;[:space:]]+")

# �������� ������ ���������� ��� ����������
df1 <- data_model_df %>%
  # � ���������� ����� ������ ��, ��� ��������� ��� ��������
  filter(!is.na(aggr_ops)) %>%
  # 1-�� ������������: �� ���������
  separate_rows(aggr_ops, sep="[,;[:space:]]+") %>%
  # 2-�� ������������: �� ���������� �����
  separate(aggr_ops, into=c("aggr_ops", "ratio_type"), sep="[:[:space:]]+") %>%  
  # ��������� ������ ������� ��������� � ��������� ���� (�����, CH)
  mutate(x=aggr_ops,
         ext_aggr_opts=case_when(
           x=="min" ~ "���, min",
           x=="max" ~ "����, max",
           x=="mean" ~ "��, avg",
           x=="sum" ~ "�����, sum",
           x=="unique" ~ "����, uniq",           
           x=="count" ~ "�����, count",
           TRUE ~ "UNKNOWN"
         )) %>%
  # �������� �� ��������� �������
  separate(ext_aggr_opts, into=c("visual_aggr_func", "ch_aggr_func")) %>%
  select(-x, -col_label) %>%
  mutate(ch_query_name=str_c(ch_aggr_func, "(", db_field, ")")) %>%
  mutate(internal_name=ch_query_name)
  
# ������� ������� ��������� ��� ��������������� ���������� ����������
df2 <- df1 %>%
  filter(!is.na(ratio_type)) %>%
  mutate(can_be_grouped=FALSE) %>%
  mutate_at(vars(visual_aggr_func), ~str_c(.x, ", % �� ������")) %>%
  mutate(internal_name=str_c(ch_aggr_func, db_field, "ratio", sep="_"))
  # mutate(internal_name=stri_join(ch_aggr_func, db_field, "ratio", sep="_", collapse=NULL))
  # mutate(internal_name={map2_chr(.$internal_name, .$db_field, ~if_else(is.na(.x), .y, .x))})


# ��������� ��� � ������ ������
res_df <- df1 %>%
  mutate(ratio_type=as.character(NA)) %>%
  bind_rows(df2) %>%
  # select(-src) %>%
  mutate(visual_var_name={map2_chr(.$human_name_rus, .$visual_aggr_func,
                                   ~if_else(.y=="", .x, stri_join(.x, ": ", .y)))}) %>%
  mutate(id=row_number())
  
var_model_df <-res_df

# ������ �������� �������
group_model_df <- data_model_df %>%
  filter(can_be_grouped) %>%
  mutate(visual_group_name=human_name_rus) %>%
  select(internal_name=db_field, visual_group_name) %>%
  mutate(id=row_number()) %>%
  # ������� ������ ������, ����������� �� �������� �������
  add_row(id=0, visual_group_name="���") %>%
  arrange(id)  


dict_df <- dplyr::union(var_model_df %>% select(name_enu=internal_name, name_rus=visual_var_name),
                       group_model_df %>% select(name_enu=internal_name, name_rus=visual_group_name))

#data <- as.list(df2$query_name)
#names(data) <- df2$visual_name


stop()

data <- setNames(as.list(var_model_df$internal_name), var_model_df$visual_var_name)

# �������� ������ ���������� � ����� SELECT
data_model %>%
  {purrr::map2_chr(.$ch_query_name, .$col_name, ~str_c(.x, " AS ", .y))} %>%
  stri_join(sep="", collapse=", ")

# ������ �������� ��������� ������ � ������. � �����������
# updateSelectizeInput(session, 'foo', choices=data, server=TRUE)


# ���� ��� ������������� � ���������� ����� �����
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
write_lines(model_json, "datamodel_new.json") # ��������� � UTF

# write_json(df, "datamodel_new.json") #, sep = "\t", fileEncoding="utf-8")


# df1 <- separate_rows(df, aggr_ops)



