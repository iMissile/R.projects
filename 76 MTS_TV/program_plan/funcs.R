parseSheet <- function(sheet_name, fname, progress, ...){
  # выносим процесс загрузки в отдельный файл для того, чтобы иметь возможность делать потом прогресс бар и логирование

  flog.info(paste0(fname, " - ", sheet_name))
  # text <- paste0("...", stri_sub(sheet_name, from=-26, to=-1))
  text <- sheet_name
  progress$inc(amount=1, detail=text)
  
  # browser()
  df0 <- read_excel(fname, sheet=sheet_name, skip=1) # пропускаем шапку

  repl_df <- tribble(
    ~pattern, ~replacement,
    "Наименование канала", "title",
    "EPG ID", "epg_id",
    "LCN", "lcn",
    "#", "row_num"
  )
  names(df0) <- stri_replace_all_fixed(names(df0),
                                       pattern = repl_df$pattern,
                                       replacement = repl_df$replacement,
                                       vectorize_all = FALSE)

  df <- df0 %>% 
    # select(title=`Наименование канала`, epg_id=`EPG ID`, genre=`Жанр`) %>%
    # возникают нюансы с кодировками имен закладок. они в 1251
    select(row_num, title, epg_id, lcn) %>%
    mutate(lcn=as.numeric(lcn)) %>%
    # filter(complete.cases(.)) %>%
    mutate(city=sheet_name)
    #mutate(row_num=row_number()+) %>% # +2 потому что удалили первую строку, а вторая ушла в заголовок

  df
} 

# stri_conv("Имя канала", from="", to="UTF-8", to_raw=FALSE)
# stri_conv("Имя канала", from="windows-1251", to="UTF-8", to_raw=FALSE)

publishToSQL <- function(clean_df) {
  # делаем экспорт в PostgreSQL ---------------------
  # Connect to a specific postgres database
  if (Sys.info()["sysname"] == "Windows") {
    dw <- config::get("media-tel")
  }else{
    dw <- config::get("cti")
  }
  
  # dbConnect из RPostgreSQL
  con <- dbConnect(dbDriver(dw$driver),
                   host = dw$host,
                   user = dw$uid,
                   password = dw$pwd,
                   port = dw$port,
                   dbname = dw$database
  )
  dbWriteTable(con, "tv_list", clean_df, overwrite = TRUE)
  dbDisconnect(con)
}