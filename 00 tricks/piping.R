library(magrittr)
library(purrr)
library(tibble)
st <- list(c(1, 2), c(3, 4), c(5, 6))
# sprintf("(%s) %s-%s-%s", list(1, 2, 3, 4)) # не работает, пишет `too few arguments`

m1 <- st %>% map(`[[`, 2)

# x %>% {f(y = nrow(.), z = ncol(.))} is equivalent to f(y = nrow(x), z = ncol(x))
# m2 <- st %>% {map(. %>% map(`[[`, 2), `^`, 2)}
m2 <- st %>% {map(., str)}
m2 <- st %>% {map(., function(n){`[[`(n, 2)})}
m2 <- st %>% {map(., function(n){cat("--\n"); dput(n); `[[`(n, 2)})}
m2 <- st %>% {map(., function(n){as.list(n) %>% map(`[[`, 1)})}

list(5, 6) %>% map(`[`, 1)
c(5, 6) %>% map(`+`, 1)




m2 <- st %>% {map(. %>% map(`[[`, 2), `^`, 2)}
m2 <- st %>% {map(. %>% map(`[[`, 2) %>% unlist(), `^`, 2)}

# %>% unlist()
m3 <- st %>% map(`^`, 2) %>% unlist()

m <- st %>% {sprintf("(%s) %s-%s-%s", stri_sub(., 1, 3), stri_sub(., 4, 6), stri_sub(., 7, 8), stri_sub(., 9, 10))}

stop()

pvis <- dir(main_path) %>% 
  paste0(main_path, .) %>% 
  map(read_xml(.) %>% map(~xml_text(xml_find_all(.,"//advp:PropertyVarietyId/wsrls:Id")))) %>% 
  unlist %>% 
  table



