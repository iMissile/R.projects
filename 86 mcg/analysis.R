library(tidyverse)
library(forcats)
library(magrittr)
library(stringi)
library(ggthemes)
library(ggrepel)
library(scales)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
library(lubridate)
library(fasttime)
library(anytime)
library(profvis)
library(tictoc)
library(microbenchmark)
library(diffobj)
# D:\iwork.GH\R.projects\72 bee_cdp\process_bee_cdr.R

windowsFonts(robotoC="Roboto Condensed")

loadGsmData <- function(){
  flist <- dir(path="./data/", pattern="stats_mcg_gsm_.*[.]csv", full.names=TRUE)
  raw_df <- flist %>% 
    # head(10) %>%
    # purrr::map_df(process_gsm, .id=NULL)
    purrr::map_dfr(read_delim, col_names=FALSE, col_types="ccc", delim=";", .id=NULL)

  # постпроцессинг, все данные как строки, это принципиально!
  # сливаем последовательные строчки
  df0 <- bind_cols(list(head(raw_df, -1), tail(raw_df, -1))) %>% 
    select(1:4)
  
  # вытащим первую строку в качестве имен
  data_names <- df0 %>%
    slice(1) %>% 
    unlist(., use.names=FALSE)
  
  df1 <- df0 %>%
    purrr::set_names(dplyr::coalesce(c("site", NA, NA, "tms"), data_names)) %>%
    mutate(idx=row_number() %% 2) %>%
    filter(idx==0)
  
  gsm_df <- df1 %>%
    mutate(timestamp=anytime(tms, tz="Europe/Moscow", asUTC=FALSE)) %>%
    mutate_at(vars(fda_failed, fda_success), as.numeric) %>%
    select(-tms, -idx) %>%
    tidyr::gather("fda_failed", "fda_success", key="fda_type", value="fda_value")
  
  gsm_df
}

loadMainData <- function(){
  flist <- dir(path="./data/", pattern="stats_mcg_main_.*[.]csv", full.names=TRUE)
  raw_df <- flist %>% 
    # head(10) %>%
    # purrr::map_df(process_gsm, .id=NULL)
    purrr::map_dfr(read_delim, col_names=FALSE, 
                   col_types=stri_flatten(rep("c", 13)), 
                   delim=";", .id=NULL)
  
  df0 <- raw_df %>%
    mutate(tms=ifelse(X2=="sms_mo", X1, NA)) %>%
    fill(tms, .direction="down")
  
  # вытащим первую строку в качестве имен
  data_names <- df0 %>%
    slice(1) %>% 
    unlist(., use.names=FALSE)
  
  df1 <- df0 %>%
    purrr::set_names(c("node", data_names[2:13], "tms")) %>%
    mutate(idx=row_number() %% 3) %>%
    filter(idx!=1)
  
  df2 <- df1 %>%
    mutate(timestamp=anytime(tms, tz="Europe/Moscow", asUTC=FALSE)) %>%
    mutate_at(vars(-node, -timestamp), as.numeric) %>%
    select(-tms, -idx) %>%
    select(timestamp, node, everything()) 
  
  main_df <- df2 %>%
    tidyr::gather(key="key", value="value", -timestamp, -node)
  
  main_df
}

tic("Loading & processing GSM")
gsm_df <- loadGsmData()
saveRDS(gsm_df, "gsm_df.rds")
toc()

tic("Loading & processing Main")
main_df <- loadMainData()
saveRDS(main_df, "main_df.rds")
toc()


# ================= визуализация  GSM результатов ======================
sub_df <- gsm_df %>%
  filter(timestamp %within% interval(dmy("11.12.2017"), dmy("12.12.2017")))

sub_df <- gsm_df

ggplot(sub_df, aes(timestamp, fda_value, color=fda_type)) +
  geom_line(linetype=1, size=0.5) +
  # geom_point(aes(fill=fda_type), shape=21, size=2, stroke=.5, alpha=0.5) +
  # geom_label(aes(label=actualvalue)) +
  # geom_label_repel(aes(label=fda_value),
  #                  fontface = 'bold', # color = 'white',
  #                  box.padding = unit(0.35, "lines"),
  #                  point.padding = unit(0.5, "lines"),
  #                  segment.color = 'grey50'
  # ) +
  scale_x_datetime(labels=date_format("%d.%m%n%H:%M", tz="Europe/Moscow"),
                   breaks=date_breaks("1 days"),
                   minor_breaks=date_breaks("6 hours")
                   ) +  
  theme_ipsum_rc(base_family="robotoC", base_size=14) +
  ylab("Показатель") +
  xlab("Дата")  


# ================= визуализация  Main результатов ======================
sub_df <- main_df %>%
  filter(timestamp %within% interval(dmy("12.12.2017"), dmy("17.12.2017"))) %>%
  filter(!(key %in% c("sms_other", "sms_other_acc", "sms_other_rej")))

sub_df <- main_df

ggplot(sub_df, aes(timestamp, value, color=node)) +
  geom_line(linetype=1, size=0.7) +
  # geom_smooth(stat="smooth") +
  # geom_point(aes(fill=node), shape=21, size=2, stroke=.5, alpha=0.5) +
  # geom_label(aes(label=actualvalue)) +
  # geom_label_repel(aes(label=fda_value),
  #                  fontface = 'bold', # color = 'white',
  #                  box.padding = unit(0.35, "lines"),
  #                  point.padding = unit(0.5, "lines"),
  #                  segment.color = 'grey50'
  # ) +
  scale_x_datetime(labels=date_format("%d.%m%n%H:%M", tz="Europe/Moscow"),
                   breaks=date_breaks("1 days")#,
                   #minor_breaks=date_breaks("12 hours")
  ) + 
  facet_wrap( ~ key, scales="free", ncol=3) +
  theme_ipsum_rc(base_family="robotoC", base_size=13) +
  ylab("Показатель") +
  xlab("Дата")  


stop()


mcg_gsm <- dir(path="./data/", pattern="stats_mcg_gsm_.*[.]csv", full.names=TRUE)

raw_df <- read_delim(mcg_gsm[[1]], col_names=FALSE, delim=';')
print(problems(raw_df), n=Inf)
# сливаем с задержкой
df <- bind_cols(list(head(raw_df, -1), tail(raw_df, -1))) %>% 
  mutate(idx=row_number() %% 2) %>%
  select(1:4)

# purrr::set_names(c("timestamp", "target")) %>%
# вытащим первую строку в качестве имен
data_names <- df %>%
  slice(1) %>% 
  unlist(., use.names=FALSE)

df1 <- df %>%
  purrr::set_names(dplyr::coalesce(c("timestamp", NA, NA, "site"), data_names))
  

# # https://stackoverflow.com/questions/42769650/using-column-index-in-dplyrs-rename
# df1 <- df %>%
#   purrr::set_names(data_names) %T>% 
#   {colnames(.)[1]="timestamp"; colnames(.)[4]="site"}
  

# diffPrint(target=gsm_df, current=m)

# выносим процесс загрузки в отдельный файл для того, чтобы иметь возможность делать потом прогресс бар и логирование
process_gsm <- function(fname, ...){
  print(fname)
  raw_df <- read_delim(fname, col_names=FALSE, delim=';')
  # сливаем с задержкой
  df <- bind_cols(list(head(raw_df, -1), tail(raw_df, -1))) %>% 
    mutate(idx=row_number() %% 2) %>%
    filter(idx==1) %>%
    select(tms=X1, site=X11, fda_success=X21, fda_failed=X31)
  # problems(df)
  s <- spec(df)
  # print(s)
  df
}

# = отладка для main данных

