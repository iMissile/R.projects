library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)
library(iterators)
library(foreach)
library(doParallel)

cat("Готовим данные")

# Генерим словарь стемминга: какой терм брать и в какой преобразовать
terms.from <-
  replicate(n = 800, expr = paste0(sample(
    x = letters,
    size = rpois(n = 1, lambda = 10),
    replace = T
  ), collapse = "")) #какой брать

terms.to <- abbreviate(terms.from, method = "left.kept") # делаем "как бы стемминг" через базовую функцию abbreviate (для примера-сойдет)

dict <-
  data.frame(terms.from,
             terms.to,
             row.names = NULL,
             stringsAsFactors = F)
head(dict)

# генерим термы вне словаря (не подлежат стеммингу)
terms.appendix <-
  replicate(n = 100, expr = paste0(sample(
    x = letters,
    size = rnorm(n = 1, mean = 30, sd = 10),
    replace = T
  ), collapse = ""))

# генерим общий корпус документов который должен быть обработан словарем: один документ = предложение из случайных слов-термов
all.terms <- replicate(n = 8000, expr =
                         paste(sort(
                           c(
                             sample(x = terms.from, size = 1, replace = F),
                             # случайное слово из terms.from
                             sample(x = terms.to, size = 1, replace = F),
                             # + случайное слово из terms.to
                             sample(x = terms.appendix, size = 1, replace = F),
                             # + случайное слово вне словаря
                             rf(n = 1, df1 = 10, df2 = 1) # + случайная цифра
                           )
                           , decreasing = T
                         )
                         , collapse = " "))

cat("Смотрим базовый подход")
# смотрим длительность базового подхода замены по словарю (86.46 sec)
all.terms2 <- all.terms
system.time(for (i in 1:nrow(dict)) {
  all.terms2 <-
    gsub(
      pattern = paste0("\\b", dict$terms.from[i], "\\b", collapse = ""),
      replacement = dict$terms.to[i],
      x = all.terms2,
      ignore.case = T,
      fixed = F
    )
})
data.frame(all.terms[1:5], all.terms2[1:5])

# смотрим длительность решения замены по словарю (525.52 sec + Null на выхRоде)
cat("Смотрим параллельный подход")
cores = detectCores()
#registerDoParallel(detectCores() - 1)
registerDoParallel(cores)
getDoParWorkers()

t <- as_tibble(all.terms)
all.terms2 <- t %>%
  # mutate(l = str_length(value), thread = ntile(l, n = cores)) %>%
  # select(-l) %>%
  mutate(thread = row_number() %% cores) %>%
  group_by(thread) %>% 
  nest()

system.time(res <-
              foreach(it = iter(all.terms2$data), .combine = 'c', .packages = 'stringr') %dopar% {
                temp.val <- it$value;
                # temp.val <- stringr::str_c(it$value, collapse = ';');
                # если так схлопываем, то потом надо достать с помощью
                # stringr::str_extract_all(temp.val, "[^;]+")
                
                matches <- purrr::map(dict$terms.from, ~ str_c("\\b", ., "\\b", collapse = ""))
                names(matches) <- purrr::map(dict$terms.to, ~ str_c("\\b", ., "\\b", collapse = ""))
                temp.val <- str_replace_all(temp.val, matches) # https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html
                # cat("----\n"); str(temp.val);
                
                # cat("----\n"); str(it);
                # for (i in 1:nrow(dict)) {
                #   temp.val <-
                #     gsub(
                #       pattern = paste0("\\b", dict$terms.from[i], "\\b", collapse = ""),
                #       replacement = dict$terms.to[i],
                #       x = temp.val,
                #       ignore.case = T,
                #       fixed = F
                #     )
                # }
                temp.val
              })