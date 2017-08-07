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
  # в переменные берем только то, что подпадает под агрегаты
  separate_rows(aggr_ops) %>%
  filter(!is.na(aggr_ops)) %>%
  mutate(visual_name=str_c("visial ", aggr_ops, "(", col_name, ")")) %>%
  mutate(query_name=str_c(aggr_ops, "(", col_name, ")"))

#data <- as.list(df2$query_name)
#names(data) <- df2$visual_name
data_model_df <- df2
data <- setNames(as.list(data_model_df$query_name), data_model_df$visual_name)


selectizeInput("selected_req", "Поля для запроса", choices=data, 
               selected=NULL, multiple=TRUE, width="100%")

# теперь заполним возможные выборы в сооотв. с метамоделью
# updateSelectizeInput(session, 'foo', choices=data, server=TRUE)
stop()

# блок для моделирования и добавления новых полей
df <- jsonlite::fromJSON("datamodel.json", simplifyDataFrame=TRUE) %>%
  mutate(can_be_grouped=FALSE)


model_json <- jsonlite::toJSON(df, pretty=TRUE)
write_lines(model_json, "datamodel_new.json")

# write_json(df, "datamodel_new.json") #, sep = "\t", fileEncoding="utf-8")


# df1 <- separate_rows(df, aggr_ops)



