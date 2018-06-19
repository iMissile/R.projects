library(tidyverse)

df <- "Item,      Store      
Item 1,   Store 1
Item 1,   Store 2
Item 1,   Store 3
Item 1,   Store 4
Item 2,   Store 1
Item 2,   Store 2
Item 2,   Store 3
Item 2,   Store 4" %>%
  read_delim(delim=",", trim_ws=TRUE)

m <- df %>% group_by(Item) %>%
  summarise(val=list(Store)) %>%
  ungroup()

# Необходимо его преобразовать в следующий вид:
# Item      Store
# Item 1    Store 1, Store 2, Store 3, Store 4
# Item 2    Store 1, Store 2, Store 3, Store 4

# либо сделать так
m <- tidyr::nest(df, Store)
dput(m)

m$data

