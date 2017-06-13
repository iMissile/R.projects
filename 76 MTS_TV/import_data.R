library(tidyverse)
library(tibble)
library(purrr)
library(magrittr)
#library(data.tree)
#library(rlist)
#library(reshape2)
#library(pipeR)
library(jsonlite)
#library(tidyjson)
#library(elastic)
#library(parsedate)
#library(fasttime)
#library(anytime)
library(microbenchmark)
library(tictoc)

# https://jsonformatter.curiousconcept.com/

if (FALSE){
fname <- "./data/head.json"
fname <- "./data/stat_fix.json"
# fname <- "./data/stat_small.json"


tic()
# заменим концы строк на `,`` и добавим шапочку и окончание для формирования семантически правильного json
# последнюю ',' надо удалить, может такое встретиться (перевод строки)
# tmp <- paste0('{"res":[', gsub("\\n", ",\n", wrecs, perl = TRUE), ']}')
tvstream <- stream_in(file("./data/stat_small.json"), pagesize = 10000)

#df <- jsonlite::fromJSON(fname, simplifyDataFrame=TRUE)
toc()

system.time(saveRDS(tvstream, "tvstream2.rds", ascii=FALSE, compress="gzip"))
}

system.time(rawdf <- readRDS("./data/tvstream2.rds"))

tic()
# руками проводим перекомпоновку
tracemem(df)
df <- select(rawdf, -`_source`) %>%
  add_column(date=rawdf$`_source`$date) %>%
  add_column(timestamp=rawdf$`_source`$`@timestamp`) %>%
  add_column(version=rawdf$`_source`$`@version`) %>%
  add_column(host=rawdf$`_source`$host) %>%
  add_column(type=rawdf$`_source`$type) %>%
  bind_cols(rawdf$`_source`$headers) %>%
  bind_cols(rawdf$`_source`$data)
toc()
# ==============

stop()
m <- rawdf[c(2,3,4), ]



# преобразуем для ясного понимания имена колонок
hdr_names <- rawdf %>%
{names(.$`_source`$headers)} %>%
  paste0("srchdr_", .)

data_names <- rawdf %>%
	{names(.$`_source`$data)} %>%
	paste0("srcdata_", .)


l <- m$`_source`$headers
setNames(l, hdr_names)

# выцепим header данные
mm <- m %>% {setNames(.$`_source`$headers, hdr_names)}

# руками проводим перекомпоновку
tmp_df <- m
df <- select(tmp_df, -`_source`) %>%
  add_column(src_date=tmp_df$`_source`$date) %>%
  add_column(src_timestamp=tmp_df$`_source`$`@timestamp`) %>%
  add_column(src_version=tmp_df$`_source`$`@version`) %>%
  add_column(src_host=tmp_df$`_source`$host) %>%
  add_column(src_type=tmp_df$`_source`$type) %>%
  bind_cols(setNames(tmp_df$`_source`$headers, hdr_names)) %>%
  bind_cols(setNames(tmp_df$`_source`$data, data_names))
  
  
  


  stri_replace_all_fixed(pattern=c("#", "-", " "), replacement=c("", "_", "_"), vectorize_all=FALSE)
names(df) <- fix_names




df <- m %>%
  {tibble(index = .$`_index`), }





stop()

setNames( 1:3, c("foo", "bar", "baz") )
# this is just a short form of
tmp <- 1:3
names(tmp) <-  c("foo", "bar", "baz")
tmp

## special case of character vector, using default
setNames(nm = c("First", "2nd"))



l <- m$`_source`$headers

dput(names(l))

h <- l %>% select_(.dots=names(.))
h <- select_(l, .dots=names(l))
rm(h)

h <- l %>% rename_(.dots=names(.), =paste0("prefix_", names(.)))

h <- l %>% rename_(names(.), names(.))
nn <- bind_cols(l, l %>% rename_(names(.)[-1], paste0("prefix_", names(.)[-1])))


nn <-  %>% 
  bind_cols(., select_(., .dots = setNames(sel_columns, sprintf("prefix_%s", sel_columns)))) 
l <- m$`_index`
