library(dplyr)
library(lubridate)
library(tidyr)
library(magrittr)
library(purrr)
library(stringi)
library(stringr)
library(tibble)
library(readxl)
library(zoo)
# а здесь попробуем поработать с randomForest
library(randomForest)


datafile <- "./data1/2016.xlsx"
# хак по считыванию типов колонок
col_types <- readxl:::xlsx_col_types(datafile)

#col_types <- rep("text", length(col_types))
raw <- read_excel(datafile,
                 sheet = "Январь",
                 col_types = col_types) #, skip = 1)

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
df2 <- df1 %>% select(one_of("Формующая часть: Угол напорного ящика", 
                             "Формующая часть: Разница скорости струи/сетки",
                             "Формующая часть: Открытие щели напорного ящика",
                             "Формующая часть: Давление на напорном ящике",
                             "Поток: Концентрация при размоле",
                             "Производительность", # выход
                             "Вес м2", # выход
                             "Артикул"
                             )) %>%
  # rename(angle = Формующая часть: Угол напорного ящика) %>%
  filter(row_number() > 6) %>% # удаляем весь верхний шлак
  filter(complete.cases(.)) # удаляем строки, содержащие пустые данные
  



write.table(names.df$name.fix, file = "names.csv", sep = ",", col.names = NA, qmethod = "double")


# =============== попробуем натянуть random forest
# берем для обучения произвольное подмножество
train <- dplyr::sample_frac(slicedata, 0.7, replace = FALSE) # replace = TRUE -- позволяет делать дублирующиеся выборки. = FALSE -- нет
# все остальное используем для тестирования
test <- dplyr::anti_join(slicedata, train)


stop()

# удаляем строки, содержащие пустые данные
df1 <- df0 %>% filter(complete.cases(.))





# теперь удалим колонки, которые имели кривые имена (NA)
#df1 <- df0 %>%
#  select(-starts_with("repaired_"))

