library(ggplot2)
library(ggdendro)
library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(scales)
library(RColorBrewer)
library(arules)
library(iterators)
library(foreach)
library(doParallel)
library(tibble)

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

getwd()
angles <- seq(from = 0.01, to = pi/2, by = 0.02)
registerDoParallel(cores = (detectCores() - 1))
getDoParWorkers()

get_raw_data <- function(fname) {
  df <- 
    foreach(it = iter(angles), .combine = rbind, .packages='readr') %do% {
      # browser()
      temp.df <- do.call(fname, list(it))
      problems(temp.df)
      temp.df$angle = it
      temp.df
    }
  
  df1 <- df %>% 
    filter(complete.cases(.)) %>%
    mutate(source_harm = harm) %>%
    mutate(harm = harm / (2 * pi * 10))

  levs <- list(categories = seq(from = 0, to = max(df1$harm), length.out = 11))
  levs$labels <- paste0(head(ceiling(levs$categories), -1), ' - ', tail(floor(levs$categories), -1))
    
  df2 <- df1 %>%
    mutate(s = value) %>%
    mutate(marker = discretize(harm, method = "fixed", categories = levs$categories, labels = levs$labels)) 
  df2
}


## Рисунок fig8

print(get_raw_data)

read_alter <- function(n){
  read_delim(paste0("./alter/finellalter(angle=", n, ").csv"), delim = ";", quote = "\"",
             # гармоника; угол; значение
             col_names = c(
               "harm",
               "angle",
               "value"
             ),
             col_types = "ddd",
             locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
             progress = interactive())
}

alter.df <- get_raw_data(read_alter)

head(alter.df)
