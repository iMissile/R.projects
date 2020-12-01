library(udpipe)
library(tidyverse)
library(readr)

#import
df <- read_delim("Тестовое задание (лингвист) - Sheet1.csv", 
                 "\t", escape_double = FALSE, col_names = FALSE, 
                 trim_ws = TRUE)
#tolower
df %>%
  mutate(X1 = tolower(X1))->
  df

#normalise data
df %>%
  mutate(X1 = gsub('"', " ", X1),
         X1 = gsub("'", " ", X1),
         X1 = gsub("«", " ", X1),
         X1 = gsub("»", " ", X1),
         X1 = gsub("ё", "е", X1),
         X1 = gsub("•", " ", X1),
         X1 = gsub("<", " ", X1),
         X1 = gsub(">", " ", X1),
         X1 = trimws(X1, which = c("left")),
         X1 = gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", X1, perl = TRUE)) ->
  df

#download language model
udmodel <- udpipe_download_model(language = 'russian-syntagrus')
udmodel_rus <- udpipe_load_model(udmodel)

#annotate
annotated <- udpipe_annotate(udmodel_rus, x = df$X1, tagger = "default", parser = "default", trace = TRUE)
annotated <- as.data.frame(annotated)

## Find 3-word phrases
#1st pattern amod+conj+nmod
deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "amod+conj+nmod", 
                           is_regex = TRUE,
                           ngram_max = 3, 
                           detailed = TRUE)
deprel %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel

#2nd pattern conj + amod + (nmod|obj)
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "conj+amod+(nmod|obj)", 
                               is_regex = TRUE,
                               ngram_max = 3, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

#3rd pattern conj + (case|nmod) + nmod
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "conj+(case|nmod)+nmod", 
                               is_regex = TRUE,
                               ngram_max = 3, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

#4rd pattern nmod + amod + nmod
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "nmod+amod+nmod", 
                               is_regex = TRUE,
                               ngram_max = 3, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

#5th pattern parataxis + (amod|nmod) + nmod
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "parataxis+(amod|nmod)+nmod", 
                               is_regex = TRUE,
                               ngram_max = 3, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

## Find 4-word phrases
#1st pattern conj + case + amod + nmod
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "conj+case+amod+nmod", 
                               is_regex = TRUE,
                               ngram_max = 4, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

#2nd pattern conj + cc + conj + nmod
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "conj+cc+conj+nmod", 
                               is_regex = TRUE,
                               ngram_max = 4, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

#3rd pattern conj+nmod+amod+nmod
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "conj+nmod+amod+nmod", 
                               is_regex = TRUE,
                               ngram_max = 4, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

#4th pattern parataxis + case + amod + nmod
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "parataxis+case+amod+nmod", 
                               is_regex = TRUE,
                               ngram_max = 4, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

#5th pattern parataxis + cc + conj + nmod
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "parataxis+cc+conj+nmod", 
                               is_regex = TRUE,
                               ngram_max = 4, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

## Find 5-word phrases
# 1st pattern conj + cc + conj + amod + nmod
add_deprel <- keywords_phrases(annotated$dep_rel, term = annotated$token, pattern = "conj+cc+conj+amod+nmod", 
                               is_regex = TRUE,
                               ngram_max = 5, 
                               detailed = TRUE)
deprel %>% 
  full_join(add_deprel, deprel,
            by = c("keyword", "ngram", "pattern", "start", "end")) %>% 
  distinct(keyword, .keep_all = TRUE)->
  deprel
rm(add_deprel)

#delete by pattern
deprel %>% 
  filter(!str_detect(keyword, pattern = ' в | для | из | их | на | об | по |
                     | при | тк| требования|альбом|ам |ами |архитектуры |бонус|
                     |владение|владение|возможность|выезд|выпуска|график|гтс|
                     |доход|душ|ем |заработ|знан|им |интересных|карьер|качества |
                     |кодекс|команд|коммерческих условий|компенсац|конкурс|
                     |культур|кухня|лидер|льгот|методик|млн|мобил|навык|наличия|
                     |налогового|нии |нию |ния |област|объема|ов |ок |оклад|ом |
                     |оплат|опыт|основ|основы|ответственность|отдых|отчетность|
                     |офис|охран|перспектив|плата|поддержкой|подобрать|понимание|
                     |правила|преми|приемке|приложений|продвинут|пункта|работах|
                     |работы|рамк|регион|режим|ремонта|рии|рынк|скидк|склад|
                     |скорость|соглашений|соц|способ|срок|стаж|стремление|строя |
                     |супруж|сфер|тки |у |уверен|ул. |устройства |федерал|футбол|
                     |характеристик |ции |цию|язык'))->
  deprel

deprel %>% 
  select (keyword, ngram, pattern, -start, -end)->
  deprel

write_csv(annotated, path = '/Volumes/Transcend/_Linguist/annotated.csv')
write_csv(deprel, path = '/Volumes/Transcend/_Linguist/deprel.csv')

