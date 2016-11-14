library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(magrittr)
library(purrr)
library(stringi)
library(stringr)
library(tibble)
library(readxl)
library(iterators)
library(foreach)
library(doParallel)
library(zoo)


data_filename <- "./data/отчет с детализацией по ќ — (056-2000815, 022-2000791, 051-2002476).xlsx"

getOksData <- function(filename, sheetname = "") {
  # хак по считыванию типов колонок
  col_types <- readxl:::xlsx_col_types(filename)
  
  #col_types <- rep("text", length(col_types))
  raw <- read_excel(filename,
                   col_types=col_types,
                   col_names=FALSE, 
                   skip = 1) # в первой строке просто написано "ќтчетна€ форма"

  # имеем проблему, колонки с NA вместо имени
  # можно писать в одно преобразование, но специально разбил на шаги
  # трансформируем колонки
  df0 <- raw %>%
    repair_names(prefix = "repaired_", sep = "")
  
  # теперь вручную сформируем названи€ колонок
  # названи€ колонок размазаны по строкам 1-3. 
  # 1-а€ -- группирующа€, 2-€ -- детализирующа€, 3-€ -- единица измерени€ (в начале таблицы там имена колонок)
  # Ќадо их слить и переименовать колонки, причем приоритет имеет строка 3, как уточн€юща€
  #name_c2 <- tidyr::gather(df0[1, ], key = name, value = name_c2) # 1-а€ колонка ушла в имена
  #name_c3 <- tidyr::gather(df0[2, ], key = name, value = name_c3) # 1-а€ колонка ушла в имена
  
  # различные виды join не подойдут, поскольку мы хотим оставить все строки вне зависимости от результата
  # сливать по именам опасно, вдруг есть дубли
  # names.df <- dplyr::full_join(name_c2, name_c3, by = "name")
  names_df <- tibble(name_c1=tidyr::gather(df0[1, ], key=name, value=v)$v,
                     name_c2=tidyr::gather(df0[2, ], key=name, value=v)$v,
                     name_c3=tidyr::gather(df0[3, ], key=name, value=v)$v) %>%
    # http://www.markhneedham.com/blog/2015/06/28/r-dplyr-update-rows-with-earlierprevious-rows-values/
   mutate(name_c1_ext=na.locf(name_c1, na.rm=FALSE)) %>% # в начале таблицы заголовки полей идут с 3-го уровн€
   mutate(name_c2_ext=na.locf(name_c2, na.rm=FALSE))
  browser()
  
  t <- names_df %>% 
    mutate(name.fix=str_c(str_replace_na(name_c1_ext, ""), # замен€ем NA на "", см хелп на str_c
                          str_replace_na(name_c2_ext, ""), 
                          str_replace_na(name_c3, ""), sep = ":: ")) %>%
    mutate(name.fix=str_replace_all(name.fix, "\\r|\\n", " ")) %>% # перевод строки
    mutate(name.fix=str_replace_all(name.fix, "\\s+", " ")) %>% # дубли пробелов
    mutate(name.fix=str_replace_all(name.fix, "^(:: )+|(:: )+$", "")) # NA строк в начале или конце
  
  dput(t$name.fix)
  write(t$name.fix, "name.fix", append=FALSE)
  
  df1 <- df0
  repl.df <- frame_data(
    ~pattern, ~replacement,
    "‘ормующа€ часть: ”гол напорного €щика", "angle_in",
    "‘ормующа€ часть: –азница скорости струи/сетки", "speed_diff_in",
    "‘ормующа€ часть: ќткрытие щели напорного €щика", "slot_in",
    "‘ормующа€ часть: ƒавление на напорном €щике", "pressure_in",
    "ѕоток:  онцентраци€ при размоле", "concentration_in",
    "ѕроизводительность", "performance_out",
    "¬ес м2", "weight_out",
    "јртикул", "mark_out"
  )
  names(df1) <- stri_replace_all_fixed(names.df$name.fix,
                                       pattern = repl.df$pattern,
                                       replacement = repl.df$replacement,
                                       vectorize_all = FALSE)
  
  
  # ¬се равно кривые имена, дубли
  names.df %>% 
    group_by(name.fix) %>% 
    filter(n()>1)
  
  df1 %<>% repair_names(prefix = "repaired_", sep = "")
  
  # выбираем только интересующие колонки
  df2 <- df1 %>% select(angle_in, speed_diff_in, slot_in, pressure_in, concentration_in,
                        performance_out, weight_out, mark_out) %>%
    filter(row_number() > 6) %>% # удал€ем весь верхний шлак
    filter(complete.cases(.)) %>% # удал€ем строки, содержащие пустые данные
    distinct() %>% # уберем идентичные строчки
    #http://stackoverflow.com/questions/27027347/mutate-each-summarise-each-in-dplyr-how-do-i-select-certain-columns-and-give
    mutate_each(funs(as.numeric), -mark_out)
  
  df2
}


# df <- foreach(it = iter(sheets), .combine = rbind, .packages='readxl') %do% {
#   temp.df <- get_month_data(datafile, it) %>% mutate(month = it)
# 
#   temp.df
# }

df <- getOksData(data_filename)

stop()
# write.table(names.df$name.fix, file = "names.csv", sep = ",", col.names = NA, qmethod = "double")


plot(df %>% dplyr::select(-mark_out, -month))


gp <- ggplot(data = test, aes(x = value, y = predicted)) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_point(size = 3, fill = "yellow", shape = 21, na.rm = TRUE) +    # White fill
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(x = "«начение", y = "ѕрогноз")

gp

stop()

