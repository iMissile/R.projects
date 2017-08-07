library(tidyverse)
library(lubridate)
library(magrittr)
library(forcats)
library(ggrepel)
library(stringi)
library(shiny)
library(jsonlite)
#library(DBI)
#library(RPostgreSQL)
library(anytime)
library(tictoc)
library(profvis)
library(microbenchmark)
library(Cairo)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
# library(debug)
library(config)


source("clickhouse.R")




df <- readr::read_delim("datamodel.csv", delim=";")
model_json <- jsonlite::toJSON(df, pretty=TRUE)
write(model_json, "datamodel.json", sep = "\t")




toJSON(mtcars, pretty=TRUE)

