library(tidyverse)
library(forcats)
library(magrittr)
library(stringi)
library(ggthemes)
library(scales)
library(RColorBrewer)
library(hrbrthemes)
library(lubridate)
library(fasttime)
library(anytime)
library(profvis)
library(tictoc)
library(microbenchmark)
library(diffobj)
# D:\iwork.GH\R.projects\72 bee_cdp\process_bee_cdr.R

mcg_gsm <- dir(path="./data/", pattern="stats_mcg_gsm_.*[.]csv", full.names=TRUE)

df <- read_delim(mcg_gsm[[1]], col_names=FALSE, delim=';')
m <- head(df, -1)

# сливаем с задержкой
gsm_df <- bind_cols(list(head(df, -1), tail(df, -1))) %>% 
  mutate(idx=row_number() %% 2) %>%
  filter(idx==1)



# diffPrint(target=gsm_df, current=m)