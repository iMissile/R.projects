library(tidyverse)
library(readxl)
library(stringi)
library(profvis)

pplan <- "./data/Гибрид_КП_DVBC_2017-05-02.xlsx"

tmp <- excel_sheets(pplan)
sheets <- tmp[!stri_detect_fixed(tmp, c('КП', 'AC'))]

parseSheet <- function(sheet_name, fname){
  
  print(paste0(fname, " - ", sheet_name))
  
  df0 <- read_excel(pplan, sheet=sheet_name, skip=1) %>% # пропускаем шапку
    # select(title=`Наименование канала`, epg_id=`EPG ID`, genre=`Жанр`) %>%
    select(row_num=`#`, title=`Наименование канала`, epg_id=`EPG ID`) %>%
    # filter(complete.cases(.)) %>%
    mutate(city=sheet_name) %>%
    #mutate(row_num=row_number()+) %>% # +2 потому что удалили первую строку, а вторая ушла в заголовок
    select(city, row_num, everything())
  
  df0
} 

# вариант 2, взят отсюда: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html
# так быстрее на 20% и памяти в 3 раза меньше требуется
# на больших объемах скорость начинает выигрывать в разы, объем памяти также, в 4-6 раз меньше.
df0 <- sheets %>%
  purrr::map_df(parseSheet, fname=pplan) %>%
  mutate(epg_id=stri_trim_left(epg_id, pattern="\\P{Wspace}")) # убрали лидирующие пробелы
# saveRDS(df, "flow_df.rds")

# почистим данные от шлака, а потом выясним пересечение
df <- df0 %>%
  filter(!is.na(epg_id)) %>%
  filter(stri_startswith_fixed(epg_id, 'epg')) %>%
  distinct()

bad_df <- anti_join(df0, df)  

write_excel_csv(df, "program_plan_fast.csv", na="NA", append=FALSE, col_names=TRUE)
# write_delim(df, "program_plan.csv", delim="\t", na="NA", append=FALSE, col_names=TRUE)
write.table(df, "program_plan_slow.csv", na="NA", append=FALSE, col.names=TRUE, row.names=FALSE, sep=";")


stop()

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
