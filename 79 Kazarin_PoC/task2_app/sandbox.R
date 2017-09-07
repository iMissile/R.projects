# Single-file Shiny apps (http://shiny.rstudio.com/articles/single-file.html)
# ќб€зательно в кодировке UTF-8
rm(list=ls()) # очистим все переменные
gc()

library(tidyverse)
library(lubridate)
library(scales)
library(forcats)
library(readxl)
library(magrittr)
library(stringi)
library(stringr)
library(futile.logger)
library(jsonlite)
library(Cairo)
library(RColorBrewer)
library(extrafont)
library(hrbrthemes)
library(DBI)
library(RPostgreSQL)
library(config)
library(shiny)
library(shinyjqui)
library(shinythemes) # https://rstudio.github.io/shinythemes/
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(anytime)
library(tictoc)
library(digest)
library(officer)

getwd()

eval(parse("./task2_app/funcs.R", encoding="UTF-8"))

df <- readRDS("./task2_app/df.rds")

m <- getRusColnames(df)
