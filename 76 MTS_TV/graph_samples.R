library(tidyverse)
library(lubridate)
library(tibble)
library(purrr)
library(magrittr)
library(forcats)
library(microbenchmark)
library(anytime)
# library(fasttime)
library(tictoc)
library(hrbrthemes)


eval(parse("funcs.R", encoding="UTF-8"))


system.time(rawdf <- readRDS("./data/tvstream3.rds"))

df <- rawdf %>%
  select(-data_date) %>% # data_date вообще почти везде NA
  filter(complete.cases(.)) %>% # выкинули строки с NA
  sample_frac(0.2) %>% # и сразу случайным образом урежем объем
  # mutate(t=as.POSIXct(date, origin='1970-01-01'))
  # mutate(t=anytime(date/1000, tz="Europe/Moscow"))
  # похоже, что timestamp = date, только формат разный. Поэтому прибьем, чтобы память не забивать
  mutate(timestamp=anytime(date/1000, tz="UTC")) %>%
  mutate(timegroup=hgroup.enum(timestamp, min_bin=60)) %>%
  select(-date)

object.size(df)/1024/1024

unq <- map(select(df, -timestamp), unique)

# посмотрим ТОП-5 передач для ТОП-9 регионов =============

# top_n применяется для каждой группы, поэтому сначала  необходимо определить ТОП N по регионам
# выберем наиболее активные регионы c позиции эфирного времени
reg_df <- df %>%
  group_by(region) %>%
  summarise(duration=sum(duration), n=n()) %>%
  top_n(9, duration) %>%
  arrange(desc(duration))

# а теперь выберем из исходного материала только данные, касающиеся этих ТОП N регионов
df1 <- df %>%
  semi_join(reg_df, by="region") %>%
  group_by(region, channelId) %>%
  summarise(duration=sum(duration)) %>%
  ungroup() %>%
  # сортировать регионы надо будет по максимальной суммарной длительности
  group_by(region) %>%
  mutate(total_duration=sum(duration)) %>%
  # top_n(5, duration) %>% # ТОП делаем по регионам
  ungroup() %>%
  arrange(desc(total_duration), desc(duration)) %>%
  # 3. Add order column of row numbers
  mutate(order=row_number())

windowsFonts(robotoC="Roboto Condensed")

# To change the order in which the panels appear, change the levels
# of the underlying factor.
# df2$CP_order <- reorder(df2$CP, df2$n)
# так тоже не работает, потому что внутри группировка идет по порядку O,S,T. А там максимумы другие.

df2 <- df1 %>% 
  group_by(region) %>% 
  top_n(5, duration)

# Гистограмма ТОП-5 программ по выбранным регионам ====================================
# https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets
gp <- ggplot(df2, aes(fct_reorder(channelId, duration, .desc=TRUE), duration)) + 
  geom_bar(stat="identity") +
  coord_flip() +
  #facet_wrap(~fct_reorder(CP, n, .desc=TRUE), scales = "free_y") +
  #facet_wrap(~CP_order, scales = "free_y") +
  facet_wrap(~fct_reorder(region, total_duration, .desc = TRUE), scales = "free") +
  # theme_ipsum_rc(base_family="robotoC", base_size=14, axis_title_size=12) +
  theme_ipsum_rc(base_size=14, axis_title_size=12) +
  theme(axis.text.x = element_text(angle=90)) +
  xlab("Дата, время") +
  ylab("Суммарное количество минут") +
  ggtitle("Телесмотрение")

gp
