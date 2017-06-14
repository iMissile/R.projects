library(tidyverse)
library(tibble)
library(purrr)
library(magrittr)
library(microbenchmark)
library(anytime)
# library(fasttime)
library(tictoc)

system.time(rawdf <- readRDS("./data/tvstream3.rds"))

df <- rawdf %>%
  select(-data_date) %>% # data_date вообще почти везде NA
  filter(complete.cases(.)) %>% # выкинули строки с NA
  sample_frac(0.2) %>% # и сразу случайным образом урежем объем
  # mutate(t=as.POSIXct(date, origin='1970-01-01'))
  # mutate(t=anytime(date/1000, tz="Europe/Moscow"))
  # похоже, что timestamp = date, только формат разный. Поэтому прибьем, чтобы память не забивать
  mutate(timestamp=anytime(date/1000, tz="UTC")) %>%
  select(-date)

object.size(df)/1024/1024

unq <- map(select(df, -timestamp), unique)

# посмотрим ТОП-5 передач для ТОП-9 регионов =============

# top_n применяется для каждой группы, поэтому сначала  необходимо определить ТОП N по регионам
# выберем наиболее активные регионы c позиции эфирного времени
reg_df <- df %>%
  group_by(region) %>%
  summarise(total_duration=sum(duration), n=n()) %>%
  top_n(9, total_duration) %>%
  arrange(desc(total_duration))

# а теперь выберем из исходного материала только данные, касающиеся этих ТОП N регионов
df2 <- df %>%
  semi_join(reg_df, by="region") %>%
  group_by(timegroup, CP, message_type) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  # сортировать контент-провайдеров надо по максимальному числу SMS (O/S/T)
  group_by(CP) %>%
  mutate(max_n=max(n)) %>%
  ungroup() %>%
  arrange(desc(max_n)) %>%
  # 3. Add order column of row numbers
  mutate(order=row_number())

