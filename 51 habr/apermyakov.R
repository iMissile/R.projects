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
# � ����� ��������� ���������� � randomForest
library(randomForest)


datafile <- "./data1/2016.xlsx"

get_month_data <- function(filename, sheetname="") {
  # ��� �� ���������� ����� �������
  ctypes <- readxl:::xlsx_col_types(filename) # �� ����� �������, �������� ��� xlsx
  ctypes <- rep("text", length(ctypes))
  #tmp <- read_excel(filename, col_names=FALSE, sheet=sheetname)
  #browser()
  #ctypes <- rep("text", ncol(tmp))

  raw <- read_excel(filename,
                   sheet=sheetname,
                   col_names=TRUE,
                   col_types=ctypes) #, skip = 1)
  
  # ����� ��������, ������� � NA ������ �����
  # ����� ������ � ���� ��������������, �� ���������� ������ �� ����
  # �������������� �������
  df0 <- raw %>%
    repair_names(prefix="repaired_", sep="")
  
  # �������� ������� ��������� �� ������� 2-3. 2-�� -- ������������, 3-� -- ��������������
  # ���� �� ����� � ������������� �������, ������ ��������� ����� ������ 3, ��� ����������
  #name_c2 <- tidyr::gather(df0[1, ], key = name, value = name_c2) # 1-�� ������� ���� � �����
  #name_c3 <- tidyr::gather(df0[2, ], key = name, value = name_c3) # 1-�� ������� ���� � �����
  
  # ��������� ���� join �� ��������, ��������� �� ����� �������� ��� ������, ��� ����������� �� ����������
  # ������� �� ������ ������, ����� ���� �����
  # names.df <- dplyr::full_join(name_c2, name_c3, by = "name")
  names.df <- tibble(name_c2=tidyr::gather(df0[1, ], key=name, value=v)$v,
                     name_c3=tidyr::gather(df0[2, ], key=name, value=v)$v) %>%
    # http://www.markhneedham.com/blog/2015/06/28/r-dplyr-update-rows-with-earlierprevious-rows-values/
    mutate(name_c2 = na.locf(name_c2)) %>%
    # ���� name_c3 = NA, �� ��������� ����������� ����� ����� ����� NA, ��� ��� �� ����� ����������
    mutate(name.fix = ifelse(is.na(name_c3), name_c2, str_c(name_c2, name_c3, sep=": "))) %>%
    mutate(name.fix = str_replace_all(name.fix, "\r", " ")) %>% # ������� ������
    mutate(name.fix = str_replace_all(name.fix, "\n", " ")) %>% # ������� ������
    mutate(name.fix = str_replace_all(name.fix, "  ", " "))
  
  df1 <- df0
  repl.df <- tribble(
    ~pattern, ~replacement,
    "��������� �����: ���� ��������� �����", "angle_in",
    "��������� �����: ������� �������� �����/�����", "speed_diff_in",
    "��������� �����: �������� ���� ��������� �����", "slot_in",
    "��������� �����: �������� �� �������� �����", "pressure_in",
    "�����: ������������ ��� �������", "concentration_in",
    "������������������", "performance_out",
    "��� �2", "weight_out",
    "�������", "mark_out"
  )
  names(df1) <- stri_replace_all_fixed(names.df$name.fix,
                                       pattern = repl.df$pattern,
                                       replacement = repl.df$replacement,
                                       vectorize_all = FALSE)
  
  
  # ��� ����� ������ �����, �����
  names.df %>% 
    group_by(name.fix) %>% 
    filter(n()>1)
  
  df1 %<>% repair_names(prefix = "repaired_", sep = "")
  
  # �������� ������ ������������ �������
  df2 <- df1 %>% select(angle_in, speed_diff_in, slot_in, pressure_in, concentration_in,
                        performance_out, weight_out, mark_out) %>%
    filter(row_number() > 6) %>% # ������� ���� ������� ����
    filter(complete.cases(.)) %>% # ������� ������, ���������� ������ ������
    distinct() %>% # ������ ���������� �������
    #http://stackoverflow.com/questions/27027347/mutate-each-summarise-each-in-dplyr-how-do-i-select-certain-columns-and-give
    mutate_each(funs(as.numeric), -mark_out)
  
  df2
}

# # ���� ��������
# tmp <- read_excel(datafile, col_names=FALSE, sheet="������")
# browser()
# ctypes <- rep("text", ncol(tmp)+4)
# #ctypes <- readxl:::xlsx_col_types(datafile)
# raw <- read_excel(datafile,
#                   sheet="������",
#                   col_names=FALSE,
#                   col_types=ctypes) #, skip = 1)
# 	
# browser()

# ������� ��� �������� ������
sheets <- c("������", "�������", "����", "������", "���", "����", 
            "����", "������", "��������", "�������", "������", "�������")

df <- foreach(it=iter(sheets), .combine=rbind, .packages='readxl') %do% {
  temp.df <- get_month_data(datafile, it) %>% mutate(month=it)

  temp.df
}

# write.table(names.df$name.fix, file = "names.csv", sep = ",", col.names = NA, qmethod = "double")


plot(df %>% dplyr::select(-mark_out, -month))

# =============== ��������� �������� random forest

# sample_frac ������������� chr ������� ������� � chr, ������� ������������� ������� ������� ����� ����������
mark.dictionary <- tibble(mark_out = levels(as.factor(df$mark_out))) %>% mutate(mark = row_number())

slicedata <- df %>% 
  select(-month) %>% # ����� ��� ��������������� �������
  distinct() %>% # ������ ���������� �������
  left_join(mark.dictionary, by = "mark_out") %>%
  # mutate_each(funs(as.factor), mark_out) %>%
  mutate(value = weight_out)

summary(slicedata)

# ����� ��� �������� ������������ ������������
train <- dplyr::sample_frac(slicedata, 0.7, replace = FALSE) # replace = TRUE -- ��������� ������ ������������� �������. = FALSE -- ���
# ��� ��������� ���������� ��� ������������
test <- dplyr::anti_join(slicedata, train)

# ��� ������������� ����������
set.seed(123)

# ������� randomfForest � 1000 ���������
rf <- randomForest(value ~ pressure_in + speed_diff_in + slot_in + angle_in + concentration_in + mark, 
                   data = train, importance = TRUE, ntree = 1000)

# How many trees are needed to reach the minimum error estimate? 
# This is a simple problem; it appears that about 100 trees would be enough.
which.min(rf$mse)
# Plot rf to see the estimated error as a function of the number of trees
plot(rf)

# ��������� �� 1-�� ������ � ����: http://bdemeshev.github.io/r_cycle/cycle_files/22_forest.html
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

# ==== ��������� ���������� ������ ������������
# http://theanalyticalminds.blogspot.ru/2015/04/part-4a-modelling-predicting-amount-of.html
test$predicted <- predict(rf, test)
test %<>% mutate(err = abs((predicted-value)/value))

gp <- ggplot(data = test, aes(x = value, y = predicted)) +
  theme_bw() +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_point(size = 3, fill = "yellow", shape = 21, na.rm = TRUE) +    # White fill
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(x = "��������", y = "�������")

gp

stop()

