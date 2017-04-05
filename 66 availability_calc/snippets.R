library(tidyverse)
library(magrittr)
library(purrr)
library(ggplot2)


PSystem <- function(n=0, m=0, lambda, t){
  # пока не векторизуем
  # t - время для которого надо провести расчет
  
  df <- tibble(k=seq(0, m)) %>%
    mutate(coeff=((n-m)*lambda)^k/factorial(k)) %>%
    mutate(elem=coeff*t^k)
  
  sum(df$elem) * exp(-(n-m)*lambda*t)
}

time <- seq(0, 100, by=5)
p <- purrr::map(time, ~ PSystem(2, 1, 0.05, .x))
qplot(time, unlist(p))
