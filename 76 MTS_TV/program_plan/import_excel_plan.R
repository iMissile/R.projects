library(tidyverse)
library(readxl)
library(stringi)
library(profvis)
library(anytime)


pplan <- "./data/Гибрид_КП_DVBC_2017-05-02_small.xlsx"
#pplan <- "./data/Гибрид_КП_DVBS_2017-04-03.xlsx"
pplan <- "./data/Гибрид_КП_IPTV_2017-04-03.xlsx"

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
pdate <- stri_extract_first_regex(pplan, pattern="\\d{4}-\\d{2}-\\d{2}")
pdate <- Sys.Date() # берем после разговора с Кириллом
# dput(anytime(date))

parseSheet <- function(sheet_name, fname){
  
  print(paste0(fname, " - ", sheet_name))
  
  df0 <- read_excel(pplan, sheet=sheet_name, skip=1) %>% # пропускаем шапку
    # select(title=`Наименование канала`, epg_id=`EPG ID`, genre=`Жанр`) %>%
    select(row_num=`#`, title=`Наименование канала`, epg_id=`EPG ID`) %>%
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
    select(row_num=`#`, title=`Наименование канала`, epg_id=`EPG ID`, timezone=`Час Зона`) %>%
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
df1 <- df0 %>%
  mutate(epg_id=stri_trim_left(epg_id, pattern="\\P{Wspace}")) %>% # убрали лидирующие пробелы
  mutate(date=pdate) %>%
  mutate(type=ptype) %>%
  select(date, row_num, epg_id, title, city, everything())

# определяем полезное подмножество ---------
# почистим данные от шлака, а потом выясним пересечение
clean_df <- df1 %>%
  filter(!is.na(epg_id)) %>%
  filter(stri_startswith_fixed(epg_id, 'epg')) %>%
  distinct()

bad_df <- anti_join(df1, clean_df)  

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
