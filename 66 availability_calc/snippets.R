library(tidyverse)
library(magrittr)
library(purrr)
library(ggplot2)


pSystem <- function(n=0, m=0, lambda, t){
  # пока не векторизуем
  # t - время для которого надо провести расчет
  
  df <- tibble(k=seq(0, m)) %>%
    mutate(coeff=((n-m)*lambda)^k/factorial(k)) %>%
    mutate(elem=coeff*t^k)
  
  sum(df$elem) * exp(-(n-m)*lambda*t)
}


lSystem <- function(n=0, m=0, lambda, t){
  # пока не векторизуем
  # t - время для которого надо провести расчет
  
  df <- tibble(k=seq(0, m)) %>%
    mutate(elem=((n-m)*lambda*t)^k/factorial(k))

  res <- (n-m)*lambda*((n-m)*lambda*t)^m/factorial(m)*sum(df$elem)
}

tSystem <- function(n=0, m=0, lambda, t){
  # пока не векторизуем
  # t - фиктивное время

  res <- (m+1)/(n-m)*(1/lambda)
}

time <- seq(0, 100, by=5)

ps <- purrr::map(time, ~ pSystem(3, 2, 0.05, .x))
qplot(time, unlist(ps))

ls <- purrr::map(time, ~ lSystem(3, 2, 0.05, .x))
qplot(time, unlist(ls))

ts <- purrr::map(time, ~ tSystem(3, 2, 0.05, .x))
qplot(time, unlist(ts))
