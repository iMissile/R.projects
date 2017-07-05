parseSheet <- function(sheet_name, fname, progress, ...){
  # выносим процесс загрузки в отдельный файл для того, чтобы иметь возможность делать потом прогресс бар и логирование

  flog.info(paste0(fname, " - ", sheet_name))
  # text <- paste0("...", stri_sub(sheet_name, from=-26, to=-1))
  text <- sheet_name
  progress$inc(amount=1, detail=text)
  
  # browser()
  df0 <- read_excel(fname, sheet=sheet_name, skip=1) # пропускаем шапку
  # вынуждены преобразовать имена колонок в UTF, поскольку shinyapp в utf
  fix_names <- names(df0) %>%
    stri_conv(from="windows-1251", to="UTF-8", to_raw=FALSE)
  names(df0) <- fix_names

  df <- df0 %>% 
    # select(title=`Наименование канала`, epg_id=`EPG ID`, genre=`Жанр`) %>%
    # возникают нюансы с кодировками имен закладок. они в 1251
    select(row_num=`#`, title=`Наименование канала`, epg_id=`EPG ID`) %>%
    # filter(complete.cases(.)) %>%
    mutate(city=sheet_name) %>%
    #mutate(row_num=row_number()+) %>% # +2 потому что удалили первую строку, а вторая ушла в заголовок
    select(city, row_num, everything())
  
  df
} 

# 