library(tidyverse)
library(magrittr)
#library(webreadr)
library(anytime)
library(stringi)

# я
fname <- "./data/access.log"
raw_df <- read_table2(fname, 
                      col_names=c("timestamp", "duration", "client_address", "result_codes", 
                                  "bytes", "request_method", "url", "user", "hierarcy_code", "type"),
                      col_types=("nicciccccc")
                      )
df <- raw_df %>%
  mutate(t=anytime(timestamp, tz="Europe/Moscow"))


c("http://yandex.ocsp-responder.com/", "www.yandex.ocsp-responder.com/sa") %>%
  stri_replace_all_regex("^(a-z)*://", "")

stop()
data <- read_log(fname)

data <- read_squid(fname)

data <- read_squid(system.file("extdata/log.squid", package="webreadr"))
