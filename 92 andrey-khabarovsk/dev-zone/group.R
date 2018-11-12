library(tidyverse)
library(magrittr)
library(readxl)


df <- read_excel("./data/group_by.xlsx")

df <- structure(list(Store = c("Store 1", "Store 2", "Store 3", "Store 3", 
                               "Store 3", "Store 3", "Store 2", "Store 2", "Store 2", "Store 2", 
                               "Store 2", "Store 2", "Store 1", "Store 1", "Store 1", "Store 1"), 
                     Date = structure(c(1514764800, 1514764800, 1514764800, 1514764800, 
                                        1514764800, 1514764800, 1514764800, 1514851200, 1514851200, 1514851200, 
                                        1514851200, 1514851200, 1514851200, 1514851200, 1514851200, 1514851200), 
                                      class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
                     Qty_1 = c(445, 445, 445, 8831, 7124, 29345, 2498, 25487, 3725, 9294, 2394, 29813, 
                               22076, 23848, 13011, 2671)), row.names = c(NA, -16L), 
                class = c("tbl_df", "tbl", "data.frame"))

t <- df %>% 
  group_by(Store) %>% 
  summarise(Qty = sum(Qty_1))
