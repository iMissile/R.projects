cmpExcelCols <- function(old_df, new_df, key_name, val_name) {
  # делаем сравнение в 2 шага (можно все в рамках одного потока исполнения):
  # 1. сливаем файлы по ключу
  # Можно поглядеть http://stat545.com/bit001_dplyr-cheatsheet.html
  values <- list(new=paste0(val_name, "_new"),
                 old=paste0(val_name, "_old"))
  
  diff_df <- new_df %>% 
    full_join(old_df, by=key_name, suffix=c("_new", "_old")) %>%
    # для упрощения жизни переименуем колонки в фиксированные имена, replyr пока не трогаем
    rename_(new_val=as.name(values$new)) %>%
    rename_(old_val=as.name(values$old)) %>%
    # поскольку NA по определению насравнимы (NA is noncomparable by != ), 
    # то для упрощения цепочки обработки заменим NA на "левое" значение
    tidyr::replace_na(replace=list(new_val=-9999, old_val=-9999))
  
  # 2. удаляем сроки с идентичными сравниваемыми значениями
  diff_df %<>% 
    filter(new_val != old_val) %>%
    # 3. оставляем только сравниваемые колонки
    select(oip, new_val, old_val)
  
  diff_df
}
