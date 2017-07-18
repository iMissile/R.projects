# генерация тестовых файлов для загрузки в CH
# Общий объем: 800 тыс абонентов * 100 переключений в сутки * 30 дней\мес * 3 месяца

rm(list=ls()) # очистим все переменные
gc()
library(tidyverse)
library(magrittr)
library(lubridate)
library(stringi)
library(anytime)
library(tictoc)

system.time(raw_df <- as_tibble(readRDS("./data/tvstream3.rds")))


# сделаем сэмпл немного поменьше
tic()
df <- raw_df %>%
  select(-data_date) %>% # data_date вообще почти везде NA
  filter(complete.cases(.)) %>% # выкинули строки с NA
  # mutate(timestamp = anytime(as.numeric(now()-seconds(as.integer(runif(n(), 0, 10*24*60*60)))))) %>%
  select(-date, -timestamp) %>%
  mutate(duration=if_else(duration<1, as.integer(runif(n(), 1, 10)), duration))
#  mutate(segment=sample(c("IPTV", "DVB-C", "DVB-S"), n(), replace=TRUE))
toc()

print(paste0("Размер объекта: ", round(as.numeric(object.size(df) / 1024 / 1024), digits = 1), "Мб"))

# system.time(saveRDS(df, "./data/tvstream4.rds", ascii = FALSE, compress = "gzip"))

object.size(df$segment)
n_distinct(df$duration)
summary(df)

if (FALSE){
  # сделаем выгрузки для Кирилла
  tic("Поиск уникальных значений")
  unq <- map(select(raw_df, -timestamp, -date, -switchEvent, - data_date, -duration), unique) %>% 
    map(sort, decreasing = T)
  toc()
  tic("Запись файлов")
  for (i in names(unq)){
    fname <- paste0("./output/", i, ".csv")
    print(fname)
    write_csv(as_tibble(unq[[i]]), fname, col_names=FALSE)
  }
  toc()
}

# сделаем сэмпл большего размера
tic()
recs <- 10^7
end_time <- as.numeric(now() + months(1))
# df_out <- tibble(timestamp=end_time-seconds(as.integer(runif(recs, 0, 3*30*24*60*60))))
df_out <- tibble(timestamp=end_time - as.integer(runif(recs, 0, 3*30*24*60*60)))

df <- raw_df %>%
  select(-data_date) %>% # data_date вообще почти везде NA
  filter(complete.cases(.)) %>% # выкинули строки с NA
  # mutate(timestamp = anytime(as.numeric(now()-seconds(as.integer(runif(n(), 0, 10*24*60*60)))))) %>%
  select(-date, -timestamp) %>%
  mutate(duration=if_else(duration<1, as.integer(runif(n(), 1, 10)), duration))
#  mutate(segment=sample(c("IPTV", "DVB-C", "DVB-S"), n(), replace=TRUE))
toc()

tic()
write_csv(df, "./data.csv.gz")
toc()

