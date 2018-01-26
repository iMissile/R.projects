library(pryr)
library(tidyverse)
library(magrittr)
library(textreadr)
# library(tm)
library(udpipe)
library(tidytext)
library(wordcloud)

# docx_doc <- system.file("docs/Yasmine_Interview_Transcript.docx", package = "textreadr")
docx_doc <- "./data/tech_req_sample.docx"

docx_doc %>%
  read_docx() %>%
  #head(-1) 
  as_tibble() -> text_df

# --------------- udpipe
# один раз загрузили
# udmodel <- udpipe_download_model(language="russian")
# udmodel <- udpipe_load_model(file = udmodel$file_model)
# udmodel <- udpipe_load_model(file="russian-ud-2.0-170801.udpipe")
# [Синтаксически размеченный корпус русского языка: информация для пользователей](http://www.ruscorpora.ru/instruction-syntax.html)
# udmodel <- udpipe_download_model(language="russian-syntagrus")
udmodel <- udpipe_load_model(file="russian-syntagrus-ud-2.0-170801.udpipe")
pryr::object_size(udmodel)

txt <- stringi::stri_flatten(text_df$value, collapse="\n")

# ! Mark that it is important that the x argument to udpipe_annotate is in UTF-8 encoding.
# https://bnosac.github.io/udpipe/docs/doc0.html

an <- udpipe_annotate(udmodel, x=txt) %>% as_tibble()

# сделаем wordcloud
an_wc <- an %>%
  filter(!(upos %in% c("CCONJ", "PUNCT", "ADP", "NUM", "ADV"))) %>%
  select(lemma) %>%
  count(lemma, sort=TRUE)

an_wc %$% wordcloud(lemma, n, min.freq=1,
          max.words=100, random.order=FALSE, 
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))


# --------------- tidytext
ss <- text_df %>%
  unnest_tokens(word, value) %>%
  count(word, sort=TRUE)

wordcloud(ss$word, ss$n, min.freq=1,
          max.words=200, random.order=FALSE, 
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))

# --------------- tm
# vc <- VCorpus(docx_doc, readerControl=list(reader=readDOC(engine=c("antiword", "executable"), AntiwordOptions="")))

