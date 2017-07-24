rm(list=ls()) # очистим все переменные
library(tidyverse)
library(magrittr)
library(forcats)
library(lubridate)
library(stringi)
library(Cairo)
library(RColorBrewer)
library(futile.logger)
library(anytime)
library(tictoc)
library(digest)
#library(rJava)
#library(ReporteRs)
library(officer)
library(extrafont)
library(hrbrthemes)
library(Unicode)


#eval(parse("funcs.R", encoding="UTF-8"))
source("funcs.R")

# очистим все warnings():
assign("last.warning", NULL, envir = baseenv())

system.time(raw_df <- as_tibble(readRDS("./data/tvstream4.rds")))
raw_df %<>% mutate(title=readr::parse_factor(programId, levels=NULL))
raw_df %<>% mutate(serial_num=as.numeric(serial))

# ====================================================================
# дополнительный препроцессинг для быстрой отладки + генерация недостающих полей  -------------
if (FALSE){
  system.time(raw_df <- readRDS("./data/tvstream3.rds"))
  
  # подберем коээфициент растяжения
  anytime(as.numeric(now()+days(1))*1.0005)

  # сделаем сэмпл немного поменьше
  df <- raw_df %>%
    select(-data_date) %>% # data_date вообще почти везде NA
    filter(complete.cases(.)) %>% # выкинули строки с NA
    sample_frac(0.2) %>% # и сразу случайным образом урежем объем
    # mutate(t=as.POSIXct(date, origin='1970-01-01'))
    # mutate(t=anytime(date/1000, tz="Europe/Moscow"))
    # похоже, что timestamp = date, только формат разный. Поэтому прибьем, чтобы память не забивать
    mutate(timestamp = anytime(date / 1000, tz = "UTC")) %>%
    # для экспериментов сознательно растяем данные на 10 дней назад
    mutate(timestamp = anytime(as.numeric(now()-seconds(as.integer(runif(n(), 0, 10*24*60*60)))))) %>%
    mutate(timegroup = hgroup.enum(timestamp, min_bin = 60)) %>%
    select(-date) %>%
    mutate(segment=sample(c("IPTV", "DVB-C", "DVB-S"), n(), replace=TRUE))

  print(paste0("Размер объекта: ", round(as.numeric(object.size(df) / 1024 / 1024), digits = 1), "Мб"))
  
  system.time(saveRDS(df, "./data/tvstream4.rds", ascii = FALSE, compress = "gzip"))
  
  raw_df <- df
}

# поглядим сводку по данным ---------------------
if (FALSE) {
  tic()
  # посчитаем количество уникальных значений в колонках
  dist_cols <- map_df(select(raw_df,-timestamp), n_distinct)
  # посчитаем и отсортируем словарные уникальные значения
  unq <- map(raw_df, unique) %>% map(sort, decreasing = T)
  # unq
  dist_cols
  toc()
}


# ====================================================================
# Генерация word файла для выгрузки с помощью пакета ReporteRs
if(FALSE){
  doc <- docx(title='Рейтинг каналов', template="./TV_report_template.docx" )
  s <- as_tibble(styles(doc)) %>% rownames_to_column()
  # browser()
  
  # doc <- addSection(doc, landscape=TRUE)
  doc <- addTitle(doc, 'Первые 80 строк данных', level = 1)
  out_df <- raw_df[1:80, ] %>% select(timestamp, region, programId, segment)
  doc <- addFlexTable(doc, vanilla.table(out_df))
  
  # выберем наиболее активные регионы c позиции эфирного времени
  reg_df <- raw_df %>%
    group_by(region) %>%
    summarise(duration=sum(duration), n=n()) %>%
    top_n(9, duration) %>%
    arrange(desc(duration))
  
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(region), duration, .desc=FALSE), duration)) +
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    #geom_text(aes(label=order), hjust=+0.5, colour="red") + # для вертикальных
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
    scale_y_log10() +
    theme_ipsum_rc(base_size=14, axis_title_size=12) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Суммарное количество минут") +
    xlab("Регион") +
    ggtitle("Статистика телесмотрения", subtitle="Топ 9 регионов") +
    coord_flip()  

  # browser()
  
  doc <- addParagraph(doc, value="Небольшая картинка")# , stylename = "Normal")
  doc <- addPlot(doc, function(){print(gp)}, width=6)
  
  
  writeDoc(doc, file="word_report.docx")
}

# ====================================================================
# Генерация word файла для выгрузки с помощью пакета officer
if(TRUE){
  # считаем данные для вставки -----------------------------------
  # выберем наиболее активные регионы c позиции эфирного времени
  reg_df <- raw_df %>%
    group_by(region) %>%
    summarise(duration=sum(duration), n=n()) %>%
    top_n(9, duration) %>%
    arrange(desc(duration))
  
  gp <- ggplot(reg_df, aes(fct_reorder(as.factor(region), duration, .desc=FALSE), duration)) +
    geom_bar(fill=brewer.pal(n=9, name="Greens")[4], alpha=0.5, stat="identity") +
    #geom_text(aes(label=order), hjust=+0.5, colour="red") + # для вертикальных
    # scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
    scale_y_log10() +
    theme_ipsum_rc(base_size=14, axis_title_size=12) +  
    theme(axis.text.x = element_text(angle=90)) +
    ylab("Суммарное количество минут") +
    xlab("Регион") +
    ggtitle("Статистика телесмотрения", subtitle="Топ 9 регионов") +
    coord_flip()  

  out_df <- as_tibble(raw_df[1:80, ]) %>%
    # select(timestamp, 'Яанал'=channelId)
    select("Канал"=channelId)
    #select(timestamp, stri_enc_toutf8('Канал')=channelId, 'Сегмент'=segment, 
    #       'Регион'=region, "Ну просто очень-очень длинный заголовок"=programId)

  # browser()
  # имена колонок надо превратить в unicode
  # nm <- names(out_df) %>% stri_conv(from="windws-1251", to="UTF-8", to_raw=FALSE)
  nm <- names(out_df)
  #stri_enc_mark(nm)
  #nm <- names(out_df) %>% 
  #  stri_conv(from="windows-1251", to="UTF-8", to_raw=FALSE) %>% 
  #  map(`Encoding<-`, "UTF-8")
  
  stri_enc_mark(nm)
  
  names(out_df) <- nm

  # browser()
  
  # создаем файл ------------------------------------------
  doc <- read_docx() %>% # read_docx(path="./TV_report_template.docx") %>%
    body_add_par(value='Первые 80 строк данных', style="heading 1") %>%
    body_add_table(value=out_df, style="table_template") %>% 
    body_add_par(value="Небольшая картинка", style="heading 1") %>%
    body_add_gg(value=gp, style = "centered") %>%
    print(target = "word_report_officer.docx")
}

stop()

# ====================================================================
# Отчет №1: "Рейтинг каналов" --------------------
if (FALSE) {
  tic()
  df0 <- raw_df %>%
    mutate(name_hash = map_chr(programId, digest::digest, algo = "xxhash64"))
  # для 250 тыс. строк на ноутбуке хэш считается ~25 сек
  toc()
  
  print(paste0("Размер объекта: ", round(as.numeric(object.size(df0) / 1024 / 1024), digits = 1), "Мб"))
  
  # посчитаем количество уникальных значений в колонках
  dist_cols <- map_df(select(df0, name_hash, programId), n_distinct)
}

# посчитаем количество уникальных значений в колонках
dist_cols <- map_df(select(raw_df, programId, serial, serial_num), n_distinct)

# ====================================================================
# промежуточные эксперименты со скоростью вычислений ---------------------
if (FALSE){
gc()
tic("unique string") # ~1.9 сек
microbenchmark::microbenchmark(
df0 <- raw_df %>%
  group_by(programId) %>%
  summarise(unique_box=n_distinct(serial)) %>%
  arrange(desc(unique_box)), times=10)
toc()
  
# а теперь через факторы
gc()
tic("unique factors") # ~1.9 сек
microbenchmark::microbenchmark(
  df0 <- raw_df %>%
  group_by(title) %>%
  summarise(unique_box=n_distinct(serial)) %>%
  arrange(desc(unique_box)), times=10)
toc()

# а теперь с числами вместо строк
gc()
tic("unique factors + serial_num") # ~0.1 сек
microbenchmark::microbenchmark(
  df0 <- raw_df %>%
    group_by(title) %>%
    summarise(unique_box=n_distinct(serial_num)) %>%
    arrange(desc(unique_box)), times=10)
toc()
}

# а теперь с числами вместо строк
gc()
tic("unique factors + serial_num") # ~0.1 сек
df0 <- raw_df %>%
  group_by(title) %>%
  summarise(unique_box=n_distinct(serial_num), 
            total_time=sum(duration),
            watch_events=n()) %>%
  arrange(desc(unique_box))
toc()


stop()



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

# переделываем группировку для корректного отображения внутри facet
# https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets
df2 <- df1 %>% 
  group_by(region) %>% 
  top_n(5, duration) %>%
  ungroup() %>%
  arrange(desc(total_duration), desc(duration)) %>%
  # mutate_at(vars(channelId, region), as.factor) %>%
  mutate(order=row_number())
  
# очистим все warnings():
assign("last.warning", NULL, envir = baseenv())

# Гистограмма ТОП-5 программ по выбранным регионам ====================================
# 45 строк и 34 фактора по передачам!
# gp <- ggplot(df2, aes(fct_reorder(channelId, duration, .desc=TRUE), duration)) +
# gp <- ggplot(df2, aes(order, duration)) +
gp <- ggplot(df2, aes(fct_reorder(as.factor(order), order, .desc = TRUE), duration)) +
# gp <- ggplot(df2, aes(x=fct_reorder(channelId, order, .desc = TRUE), y=duration)) +  
  geom_bar(fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
  # geom_text(aes(label=order), vjust=-0.5, colour="red") + # для вертикальных
  geom_text(aes(label=order), hjust=+0.5, colour="red") + # для вертикальных
  scale_x_discrete("Передача", breaks=df2$order, labels=df2$channelId) +
  #scale_x_discrete("Передача", labels=df2$channelId)
  #scale_x_manual("Передача", values=df2$channelId)
  # scale_x_discrete(labels=channelId)) +
  #facet_wrap(~fct_reorder(CP, n, .desc=TRUE), scales = "free_y") +
  #facet_wrap(~CP_order, scales = "free_y") +
  facet_wrap(~fct_reorder(region, total_duration, .desc = TRUE), scales = "free") +
  theme_ipsum_rc(base_size=14, axis_title_size=12) +  
  theme(axis.text.x = element_text(angle=90)) +
  ylab("Суммарное количество минут") +
  ggtitle("Статистика телесмотрения", subtitle="Топ 5 каналов для Топ 9 регионов") +
  coord_flip()


gp


