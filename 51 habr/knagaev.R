library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)

df <- tibble(t = c("36753", "37328", "37405", "37433", "30335", "30244", "30468", "31098", "35426", "37287"))

df1 <- df %>% 
  mutate(date = as.POSIXct((as.numeric(.[[1]]) - 25569) * 86400, tz = "GMT", origin = "1970-01-01"))


# можно использовать еще и unname()
m <- purrr::map_at(df, c(1), function(x) as.POSIXct((as.numeric(x) - 25569) * 86400, tz = "GMT", origin = "1970-01-01"))
