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
# а здесь попробуем поработать с randomForest
library(randomForest)


datafile <- "./data1/2016.xlsx"

get_month_data <- function(filename, sheetname = "") {
  # хак по считыванию типов колонок
  col_types <- readxl:::xlsx_col_types(datafile)
  
  #col_types <- rep("text", length(col_types))
  raw <- read_excel(filename,
                   sheet = sheetname,
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
  df2 <- df1 %>% select(angle_in, speed_diff_in, slot_in, pressure_in, concentration_in,
                        performance_out, weight_out, mark_out) %>%
    filter(row_number() > 6) %>% # удаляем весь верхний шлак
    filter(complete.cases(.)) %>% # удаляем строки, содержащие пустые данные
    distinct() %>% # уберем идентичные строчки
    #http://stackoverflow.com/questions/27027347/mutate-each-summarise-each-in-dplyr-how-do-i-select-certain-columns-and-give
    mutate_each(funs(as.numeric), -mark_out)
  
  df2
}

# соберем все страницы вместе
sheets <- c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", 
            "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь")

df <- foreach(it = iter(sheets), .combine = rbind, .packages='readxl') %do% {
  temp.df <- get_month_data(datafile, it) %>% mutate(month = it)

  temp.df
}

# write.table(names.df$name.fix, file = "names.csv", sep = ",", col.names = NA, qmethod = "double")


plot(df %>% dplyr::select(-mark_out, -month))

# =============== попробуем натянуть random forest

# sample_frac разворачивает chr факторы обратно в chr, поэтому принудительно сделаем словарь типов арткикулов
mark.dictionary <- tibble(mark_out = levels(as.factor(slicedata$mark_out))) %>% mutate(mark = row_number())

slicedata <- df %>% 
  select(-month) %>% # месяц для прогнозирования неважен
  distinct() %>% # уберем идентичные строчки
  left_join(mark.dictionary, by = "mark_out") %>%
  # mutate_each(funs(as.factor), mark_out) %>%
  mutate(value = weight_out)

summary(slicedata)

# берем для обучения произвольное подмножество
train <- dplyr::sample_frac(slicedata, 0.7, replace = FALSE) # replace = TRUE -- позволяет делать дублирующиеся выборки. = FALSE -- нет
# все остальное используем для тестирования
test <- dplyr::anti_join(slicedata, train)

# для повторяемости результата
set.seed(123)

# создаем randomfForest с 1000 деревьями
rf <- randomForest(value ~ pressure_in + speed_diff_in + slot_in + angle_in + concentration_in + mark, 
                   data = train, importance = TRUE, ntree = 1000)

# How many trees are needed to reach the minimum error estimate? 
# This is a simple problem; it appears that about 100 trees would be enough.
which.min(rf$mse)
# Plot rf to see the estimated error as a function of the number of trees
plot(rf)

# посмотрим на 1-ое дерево в лесу: http://bdemeshev.github.io/r_cycle/cycle_files/22_forest.html
# tree1 <- getTree(rf, 1, labelVar = TRUE)
# head(as_tibble(tree1))

# Using the importance()  function to calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf)[,1], decreasing = TRUE), optional = T)
names(imp) <- "% Inc MSE"
imp

# As usual, predict and evaluate on the test set
test.pred.forest <- predict(rf, test)
RMSE.forest <- sqrt(mean((test.pred.forest - test$value)^2))
RMSE.forest

MAE.forest <- mean(abs(test.pred.forest - test$value))
MAE.forest

# ==== отобразим графически данные предсказания
# http://theanalyticalminds.blogspot.ru/2015/04/part-4a-modelling-predicting-amount-of.html
test$predicted <- predict(rf, test)
test %<>% mutate(err = abs((predicted-value)/value))

gp <- ggplot(data = test, aes(x = value, y = predicted)) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_point(size = 3, fill = "yellow", shape = 21, na.rm = TRUE) +    # White fill
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(x = "Значение", y = "Прогноз")

gp

stop()

