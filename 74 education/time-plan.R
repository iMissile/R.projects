library(tidyverse)
library(stringi)
#для emeditor
#    Icon exercise \w+
# \d+ xp   -> \\n

cnames <- c("task", "duration_min")

# по умолчанию readr принимает в utf8
# смотрим кодировки 1251
cl <- as_tibble(stri_enc_list(simplify=TRUE)) %>%
  filter(stri_detect_fixed(value, "1251"))

ggplot_tt <- read_delim("Что такое ggplot?|5\n
Books & Resources|5\n
Grammar of Graphics|5\n
ggplot2|5",
  delim="|", col_names=cnames, col_types="cn", 
  locale=locale(encoding="cp1251"), trim_ws=TRUE) %>% 
  mutate(topic="Introduction")
