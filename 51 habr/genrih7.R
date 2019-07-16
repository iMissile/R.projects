load(file="intermediate_result_sku.rds")

# extract brends
data.sku <- data.sku %>% mutate(sku.name2=str_replace_all(string = sku.name,pattern = '["ЂУї]',replacement = '"'))
library(tictoc)
# use stringr
library(tidyverse)
tic()
data.sku <- data.sku %>% mutate(sku.brend=str_extract_all(data.sku$sku.name2[1:5],pattern = '"([^"]+){0,}"',simplify = F))
toc() # за половину дн€ так и не получил результата (так же раздел€л на группы по 200 строк с отправкой в foreach - безрезультатно)
# use qdap
library(qdapRegex)
tic()
data.sku <- data.sku %>% mutate(sku.brend=rm_between(data.sku$sku.name2, left = '"', right = '"', extract=TRUE,fixed = T))
toc() # 0.49 sec!!!