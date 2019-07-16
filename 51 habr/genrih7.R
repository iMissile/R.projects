library(tidyverse)
library(magrittr)
library(qdapRegex)
library(tictoc)

load(file = here::here("data4", "intermediate_result_sku.rds"))

# extract brends
data.sku %<>% 
  mutate(sku.name2 = str_replace_all(string = sku.name, pattern = '["ЂУї]', replacement = '"'))

# use stringr
# m <- str_extract_all(data.sku$sku.name2, pattern = '"([^"]+){0,}"', simplify = F)

# use stringi
m <- stringi::stri_extract_all_regex(data.sku$sku.name2, pattern = '"([^"]+)"', simplify = F)

# tic()
# data.sku2 <- data.sku %>% 
#   mutate(sku.brend = str_extract_all(data.sku$sku.name2, pattern = '"([^"]+){0,}"', simplify = F))
# toc() # за половину дн€ так и не получил результата (так же раздел€л на группы по 200 строк с отправкой в foreach - безрезультатно)
# use qdap
tic()
data.sku3 <- data.sku %>% 
  mutate(sku.brend = rm_between(data.sku$sku.name2, left = '"', right = '"', extract=TRUE, fixed = T))
toc() # 0.49 sec!!!


bench::mark(
  stringr = stringr::str_extract_all(data.sku$sku.name2, pattern = '"([^"]+)"', simplify = F),
  stringi = stringi::stri_extract_all_regex(data.sku$sku.name2, pattern = '"([^"]+)"', simplify = F),
  qdap = qdapRegex::rm_between(data.sku$sku.name2, left = '"', right = '"', extract=TRUE, fixed = T),
  re2r = re2r::re2_extract_all(data.sku$sku.name2, pattern = '"([^"]+)"', parallel = F),
  check = FALSE
)
