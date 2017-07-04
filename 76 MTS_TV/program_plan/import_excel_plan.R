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
    select(title=`Наименование канала`, epg_id=`EPG ID`) %>%
    filter(complete.cases(.)) %>%
    mutate(city=sheet_name) %>%
    select(city, everything())
  
  df0
} 

# вариант 2, взят отсюда: http://readxl.tidyverse.org/articles/articles/readxl-workflows.html
# так быстрее на 20% и памяти в 3 раза меньше требуется
# на больших объемах скорость начинает выигрывать в разы, объем памяти также, в 4-6 раз меньше.
df <- sheets %>%
  purrr::map_df(parseSheet, fname=pplan)
# saveRDS(df, "flow_df.rds")

write_excel_csv(df, "program_plan_fast.csv", na="NA", append=FALSE, col_names=TRUE)
# write_delim(df, "program_plan.csv", delim="\t", na="NA", append=FALSE, col_names=TRUE)
write.table(df, "program_plan_slow.csv", na="NA", append=FALSE, col.names=TRUE, row.names=FALSE, sep=";")

stop()
# ---------------
t <- read_excel(pplan, sheet=sheets[2], skip=1) # пропускаем шапку
df0 <- t %>%
  select(title=`Наименование канала`, epg_id=`EPG ID`, genre=`Жанр`) %>%
  filter(complete.cases(.))
