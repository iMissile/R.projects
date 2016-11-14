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


datafile <- "./data/отчет с детализацией по ОКС (056-2000815, 022-2000791, 051-2002476).xlsx"

getOksData <- function(filename, sheetname = "") {
  # хак по считыванию типов колонок
  col_types <- readxl:::xlsx_col_types(filename)
  
  #col_types <- rep("text", length(col_types))
  raw <- read_excel(filename,
                   sheet = sheetname,
                   col_types = col_types) #, skip = 1)

  return(raw)
    
  # имеем проблему, колонки с NA вместо имени
  # можно писать в одно преобразование, но специально разбил на шаги
  # трансформируем колонки
  df0 <- raw %>%
    repair_names(prefix = "repaired_", sep = "")
  
  # названия колонок размазаны по строкам 2-3. 2-ая -- группирующая, 3-я -- детализирующая
  # Надо их слить и переименовать колонки, причем приоритет имеет строка 3, как уточняющая
  #name_c2 <- tidyr::gather(df0[1, ], key = name, value = name_c2) # 1-ая колонка ушла в имена
  #name_c3 <- tidyr::gather(df0[2, ], key = name, value = name_c3) # 1-ая колонка ушла в имена
  
  # различные виды join не подойдут, поскольку мы хотим оставить все строки вне зависимости от результата
  # сливать по именам опасно, вдруг есть дубли
  # names.df <- dplyr::full_join(name_c2, name_c3, by = "name")
  names.df <- tibble(name_c2 = tidyr::gather(df0[1, ], key = name, value = v)$v,
                     name_c3 = tidyr::gather(df0[2, ], key = name, value = v)$v) %>%
    # http://www.markhneedham.com/blog/2015/06/28/r-dplyr-update-rows-with-earlierprevious-rows-values/
    mutate(name_c2 = na.locf(name_c2)) %>%
    mutate(name.fix = ifelse(is.na(name_c3), name_c2, str_c(name_c2, name_c3, sep = ": "))) %>%
    mutate(name.fix = str_replace_all(name.fix, "\r", " ")) %>% # перевод строки
    mutate(name.fix = str_replace_all(name.fix, "\n", " ")) %>% # перевод строки
    mutate(name.fix = str_replace_all(name.fix, "  ", " "))
  
  # если name_c3 = NA, то результат объединения строк также будет NA, нас это не очень устраивает
  
  df1 <- df0
  repl.df <- frame_data(
    ~pattern, ~replacement,
    "Формующая часть: Угол напорного ящика", "angle_in",
    "Формующая часть: Разница скорости струи/сетки", "speed_diff_in",
    "Формующая часть: Открытие щели напорного ящика", "slot_in",
    "Формующая часть: Давление на напорном ящике", "pressure_in",
    "Поток: Концентрация при размоле", "concentration_in",
    "Производительность", "performance_out",
    "Вес м2", "weight_out",
    "Артикул", "mark_out"
  )
  names(df1) <- stri_replace_all_fixed(names.df$name.fix,
                                       pattern = repl.df$pattern,
                                       replacement = repl.df$replacement,
                                       vectorize_all = FALSE)
  
  
  # Все равно кривые имена, дубли
  names.df %>% 
    group_by(name.fix) %>% 
    filter(n()>1)
  
  df1 %<>% repair_names(prefix = "repaired_", sep = "")
  
  # выбираем только интересующие колонки
  df2 <- df1 %>% select(angle_in, speed_diff_in, slot_in, pressure_in, concentration_in,
                        performance_out, weight_out, mark_out) %>%
    filter(row_number() > 6) %>% # удаляем весь верхний шлак
    filter(complete.cases(.)) %>% # удаляем строки, содержащие пустые данные
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

# write.table(names.df$name.fix, file = "names.csv", sep = ",", col.names = NA, qmethod = "double")


plot(df %>% dplyr::select(-mark_out, -month))


gp <- ggplot(data = test, aes(x = value, y = predicted)) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_point(size = 3, fill = "yellow", shape = 21, na.rm = TRUE) +    # White fill
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(x = "Значение", y = "Прогноз")

gp

stop()

