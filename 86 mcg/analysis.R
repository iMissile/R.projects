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

tic("Parsing GSM")
flist <- dir(path="./data/", pattern="stats_mcg_gsm_.*[.]csv", full.names=TRUE)
df0 <- flist %>% 
  # head(10) %>%
  purrr::map_df(process_gsm, .id=NULL)
toc()

gsm_df <- df0 %>%
  mutate(timestamp=anytime(tms, tz="Europe/Moscow", asUTC=FALSE)) %>%
  mutate_at(vars(fda_failed, fda_success), as.numeric) %>%
  select(-tms) %>%
  tidyr::gather("fda_failed", "fda_success", key="fda_type", value="fda_value")

tic()  
saveRDS(gsm_df, "gsm_df.rds")
toc()

# нарисуем график по gsm
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


stop()


mcg_gsm <- dir(path="./data/", pattern="stats_mcg_gsm_.*[.]csv", full.names=TRUE)

raw_df <- read_delim(mcg_gsm[[1]], col_names=FALSE, delim=';')
print(problems(raw_df), n=Inf)
# сливаем с задержкой
df <- bind_cols(list(head(raw_df, -1), tail(raw_df, -1))) %>% 
  mutate(idx=row_number() %% 2) %>%
  select(1:4)

# purrr::set_names(c("timestamp", "target")) %>%



# diffPrint(target=gsm_df, current=m)