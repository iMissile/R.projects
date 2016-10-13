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



# =========================
library(tidyverse) #seq_along
library(tibble)
library(iterators)
library(foreach)
library(microbenchmark)

x <- tibble(a = c(11, 21, 31), b = c(21, 22, 23))
y <- as.matrix(x)

# делаем выборку диагональных элементов матрицы
microbenchmark(res1 <- y[row(x) == col(x)])

# %dopar% -- запуск параллельного процессинга
microbenchmark(res2 <- foreach(it = iter(1:min(dim(y))), .combine = c) %do% { y[[it, it]] })


# =============================
mod <- lm(mpg ~ wt, data = mtcars)
str(mod$model)
features1 <- attributes(mod)
# features1
attr(mod$model, "terms")

features2 <- attributes(mod$model)
features2
attributes(features2$terms)$predvars
