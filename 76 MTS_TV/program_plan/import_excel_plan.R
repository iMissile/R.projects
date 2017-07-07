library(tidyverse)
library(readxl)
library(stringi)
library(profvis)
library(anytime)
library(DBI)
library(RPostgreSQL)
# library(config)
packageVersion("dplyr")


pplan <- "./data/Гибрид_КП_DVBC_2017-05-02_small.xlsx"
#pplan <- "./data/Гибрид_КП_DVBS_2017-04-03.xlsx"
#pplan <- "./data/Гибрид_КП_IPTV_2017-04-03.xlsx"

tmp <- excel_sheets(pplan)
sheets <- tmp[!stri_detect_fixed(tmp, c('КП', 'AC'))]
# sheets <- tmp[!stri_detect_fixed(tmp, c('AC'))]

# определяем тип файла --------------------
ptype <- case_when(
  "IPTV" %in% sheets  ~ "iptv",
  "DVB-S" %in% sheets ~ "dvbs",
  length(sheets) > 8 ~ "dvbc",
  TRUE                ~ "unknown"
)

ptype

# теперь вытащим дату ---------------------
timestamp <- stri_extract_first_regex(pplan, pattern="\\d{4}-\\d{2}-\\d{2}")
timestamp <- Sys.Date() # берем после разговора с Кириллом
timestamp <- Sys.time()
# dput(anytime(date))

parseSheet <- function(sheet_name, fname){
  
  print(paste0(fname, " - ", sheet_name))
  
  df0 <- read_excel(pplan, sheet=sheet_name, skip=1) %>% # пропускаем шапку
    # select(title=`Наименование канала`, epg_id=`EPG ID`, genre=`Жанр`) %>%
    select(row_num=`#`, title=`Наименование канала`, epg_id=`EPG ID`, lcn=`LCN`) %>%
    mutate(lcn=as.numeric(lcn)) %>%
    # filter(complete.cases(.)) %>%
    mutate(city=sheet_name)
    #mutate(row_num=row_number()+) %>% # +2 потому что удалили первую строку, а вторая ушла в заголовок
  
  df0
} 

# обойдемся без вложенных else, нет смысла усложнять
if(ptype == "dvbc") {
  # вариант 2, взят отсюда: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html
  # так быстрее на 20% и памяти в 3 раза меньше требуется
  # на больших объемах скорость начинает выигрывать в разы, объем памяти также, в 4-6 раз меньше.
  df0 <- sheets %>%
    purrr::map_df(parseSheet, fname = pplan) %>%
    mutate(timezone=0)
}
if(ptype == "dvbs") {
  df0 <- read_excel(pplan, sheet="DVB-S", skip=1) %>% # пропускаем шапку
    select(row_num=`#`, title=`Наименование канала`, epg_id=`EPG ID`, timezone=`Час Зона`, lcn=`LCN`) %>%
    filter(!is.na(lcn)) %>% # отсекаем пояснения и легенду внизу таблицы    
    mutate(city='') %>%
    mutate(timezone=as.numeric(stri_extract_first_regex(timezone, pattern="\\d+"))) %>%
    replace_na(list(timezone=0))
}
if(ptype == "iptv") {
  df0 <- read_excel(pplan, sheet="IPTV", skip=1) %>% # пропускаем шапку
    select(row_num=`#`, title=`Название канала`, epg_id=`EPG ID`, lcn=`Позиция (LCN)`) %>%
    filter(!is.na(lcn)) %>% # отсекаем пояснения и легенду внизу таблицы
    mutate(city='') %>%
    mutate(timezone=0)
}


# общий постпроцессинг --------------
raw_df <- df0 %>%
  # если мы заботимся о чистоте мастер данных, то не надо отбрасывать лидирующие пробелы. это ошибка
  # mutate(epg_id=stri_trim_left(epg_id, pattern="\\P{Wspace}")) %>% # убрали лидирующие пробелы
  filter(title!="Резерв") %>%
  mutate(timestamp=timestamp) %>%
  mutate(type=ptype) %>%
  select(timestamp, row_num, epg_id, title, city, everything())

# определяем полезное подмножество ---------
# для улучшения диагностики сначала отмаркируем колонку с ошибками
raw_df %<>%
  mutate(error=case_when(
    is.na(epg_id) ~ "Отсутствует EPG ID",
    !stri_startswith_fixed(epg_id, 'epg') ~ "EPG id начинается не с 'epg'"
  ))

bad_df <- filter(raw_df, !is.na(error))  

clean_df <- raw_df %>%
  filter(is.na(error)) %>%
  select(-error) %>%
  distinct()


# делаем экспорт в PostgreSQL ---------------------
# Connect to a specific postgres database 
# con <- dbConnect(RPostgres::Postgres(), dbname='channel_list',
# con <- dbConnect(dbDriver("PostgreSQL"), dbname='channel_list',
#                  host = '10.0.0.177', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
#                  port = 5432, # or any other port specified by your DBA
#                  user = 'puser',
#                  password = 'puser')

dw <- config::get("datawarehouse")

# dbConnect из RPostgreSQL
con <- dbConnect(dbDriver(dw$driver),
                 host = dw$host,
                 user = dw$uid,
                 password = dw$pwd,
                 port = dw$port,
                 dbname = dw$database
)


dbWriteTable(con, "tv_list", clean_df, overwrite=TRUE)
dbListTables(con)

dbListFields(con, "tv_list")

# принудительно загоняем кодировку сгруженных данных в unicode
m <- dbReadTable(con, "tv_list") %>%
  mutate_if(is.character, `Encoding<-`, "UTF-8")

# Disconnect from the database
dbDisconnect(con)

if(FALSE){
  # https://stackoverflow.com/questions/21392786/utf-8-unicode-text-encoding-with-rpostgresql
  dbGetQuery(con, "SHOW CLIENT_ENCODING") 
  dbGetQuery(con, "SHOW CLIENT_ENCODING") 

  dbRemoveTable(con, "tv_list")
  # dbWriteTable(con, "tv_list", select(clean_df, -date))
  clean_df$timestamp <- Sys.time()
  
    # проверяем мапирование типов данныхЖ https://cran.r-project.org/web/packages/DBI/vignettes/spec.html#_examples_12
  anytime(Sys.Date())
  dbDataType(con, 1)
  dbDataType(con, Sys.Date())
  dbDataType(con, Sys.time())
  # postgresqlpqExec(con, "SET client_encoding = 'windows-1251'") # устанавливам кодировку возврата

  # Convert factors to characters
  m2 <- m %>%
    mutate_if(is.character, stri_conv, from="UTF-8", to="windows-1251", to_raw=FALSE)
  # сравним проблемы с кодировкой
  res <- tibble(before=clean_df$title, after=m2$title) %>%
    mutate(diff=(before!=after)) %>%
    filter(diff)
}

stop()

write_excel_csv(df, "program_plan_fast.csv", na="NA", append=FALSE, col_names=TRUE)
# write_delim(df, "program_plan.csv", delim="\t", na="NA", append=FALSE, col_names=TRUE)
write.table(df, "program_plan_slow.csv", na="NA", append=FALSE, col.names=TRUE, row.names=FALSE, sep=";")


na_df <- df %>%
  filter(is.na(epg_id))

badepg_df <- df %>%
  # filter(!str_detect(epg_id, '\\d{3}-\\d{7}$')) %>%
  filter(!stri_startswith_fixed(new_epg_id, 'epg'))


df2 <- df %>% 
  filter(complete.cases(.)) %>%
  distinct()


# комментарии от Кирилла
# 1. каналы с NA было бы неплохо логировать. хорошо если там какой-то мусор, а вот если где-то что-то забыли - это можно высказать авторам эксельки
# 2. ещё выкидываем каналы, у которых вместо epg_id знак "-"

stop()
# ---------------
t <- read_excel(pplan, sheet=sheets[2], skip=1) # пропускаем шапку
df0 <- t %>%
  select(title=`Наименование канала`, epg_id=`EPG ID`, genre=`Жанр`) %>%
  filter(complete.cases(.))
