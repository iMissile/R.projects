library(tidyverse)
library(tibble)
library(purrr)
library(magrittr)
library(microbenchmark)
library(tictoc)

system.time(rawdf <- readRDS("./data/tvstream3.rds"))