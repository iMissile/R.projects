library(dplyr)
library(lubridate)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)
library(readxl)
library(zoo)

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

# df1 <- df0 %>% select(one_of("Смена")) %>% mutate(n = row_number())
df1 <- df0 %>% filter(row_number() > 6)
# пропускаем первые 6 строк (заголовки и служебный хлам)
names(df1) <- names.df$name.fix

write.table(df1, file = "names.csv", sep = ",", col.names = NA, qmethod = "double")


# удаляем строки, содержащие пустые данные
df1 <- df0 %>% filter(complete.cases(.))





# теперь удалим колонки, которые имели кривые имена (NA)
#df1 <- df0 %>%
#  select(-starts_with("repaired_"))

