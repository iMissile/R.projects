library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(tibble)
library(ggplot2)


tb_heat_years <- readRDS("tb_heat_years.rds") %>% ungroup() # !!!

# все годы - вспомогательная для right_join - 44 штуки
all_years <- as.character(seq(from = min(as.integer(tb_heat_years$OldPriorYear)), 
                              to = max(as.integer(tb_heat_years$OldPriorYear))))

# все параграфы - вспомогательная для right_join - 88 штук
all_facets <- tb_heat_years %>% .$facet %>% unique()

# все комбинации года и параграфа - 3872 записи = 44 * 88
tb_full_years <- as_tibble(expand.grid(OldPriorYear = all_years,
                                       facet = all_facets,
                                       KEEP.OUT.ATTRS = FALSE,
                                       stringsAsFactors = FALSE))


# right_join отрабатывает правильно - 3872 записи 
tb_heat_years_join <-
  tb_heat_years %>% 
  right_join(tb_full_years, by = c("OldPriorYear", "facet"))

# tb_heat_years уже мутная... что там за комбинации?
m <- tb_heat_years %>% distinct(OldPriorYear, facet, .keep_all = TRUE)
m <- tb_heat_years %>% group_by(OldPriorYear, facet) %>% filter(n()>1) %>% summarize(n=n())

m <- tb_heat_years %>% expand(OldPriorYear, facet)
# кривизна возникает на уровне expand. В мануале говорится про `For continuous variables`
tb_heat_years %>% distinct(OldPriorYear) # имеем 37 строк
m <- tb_heat_years %>% expand(OldPriorYear=all_years) # имеем 1628 строк. 
# 1628 = 37*44, т.е. возникает ощущение, что раскрытие каждого уникального значение идет в указанный вектор!

m <- tb_heat_years %>% expand(OldPriorYear=all_years, facet=all_facets) # имеем 3872 строк. 


# complete попытка №1 - 143264 записи, размножаются годы, 3256 * 44 (3256 из начальной tb_heat_years и 44 года)
tb_heat_years_complete1 <-
  tb_heat_years %>% 
  complete(OldPriorYear = all_years, facet = all_facets)

tb_heat_years_complete1_2 <-
  tb_heat_years %>% 
  complete(OldPriorYear = all_years) # %>%
  #complete(facet = all_facets)

m <- filter(tb_heat_years_complete1_2, OldPriorYear=="1973" & facet=="3.2.6")


#IS: А что именно у нас не так?
# t <- anti_join(tb_heat_years_complete1, tb_heat_years_join, by=c("OldPriorYear"="OldPriorYear", "facet"="facet"))
t <- anti_join(tb_heat_years_complete1, tb_heat_years_join)
# полно дублей
filter(tb_heat_years_complete1, OldPriorYear=="1973" & facet=="3.2.6")

# complete попытка №2 - те же 143264 записи
tb_heat_years_complete2 <-
  tb_heat_years %>% 
  complete(OldPriorYear = as.character(seq(from = min(as.integer(tb_heat_years$OldPriorYear)), to = max(as.integer(tb_heat_years$OldPriorYear)))), facet)

#IS: сделаем дебаг
ff <- function(x) {
  browser()
  x
}
# complete попытка №3 - 3256 записей (как в начальной tb_heat_years)
tb_heat_years_complete3 <-
  tb_heat_years %>% 
  complete(OldPriorYear = as.character(ff(full_seq(as.integer(OldPriorYear), 1))), facet = all_facets)

tb_heat_years_complete3_2 <-
  tb_heat_years %>% 
  complete(OldPriorYear = as.character(full_seq(as.integer(.$OldPriorYear), 1)), facet = all_facets)


#IS: а что мы получили в качестве года?
distinct(tb_heat_years_complete3, OldPriorYear)

# при этом получение последовательности лет правильное - 44 года 
full_seq(as.integer(tb_heat_years$OldPriorYear), 1)
dput(.Last.value)


#IS: А если так?
tb_heat_years_complete4 <-
  tb_heat_years %>% 
  complete(OldPriorYear = as.character(full_seq(as.integer(.$OldPriorYear), 1)), facet = all_facets)

