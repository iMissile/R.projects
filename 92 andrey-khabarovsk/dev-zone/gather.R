library(tidyverse)
library(magrittr)

# Пример 1 ======================
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


# Пример 2 ======================

df1 <- "Item Store
Item1 Store1
Item1 Store2
Item1 Store5
Item1 Store6
Item2 Store1
Item2 Store2
Item2 Store3" %>%
  read_delim(delim=" ", trim_ws=TRUE) %>%
  `attr<-`("spec", NULL)

df2 <- "Item Store
Item1 Store1
Item1 Store2
Item1 Store3
Item1 Store4
Item1 Store5
Item1 Store6
Item2 Store1
Item2 Store2
Item2 Store3
Item2 Store4
Item2 Store5
Item2 Store6
Item2 Store7
Item2 Store8" %>%
  read_delim(delim=" ", trim_ws=TRUE) %>%
  `attr<-`("spec", NULL)

semi_join(df2, df1, by=c("Item", "Store"))

