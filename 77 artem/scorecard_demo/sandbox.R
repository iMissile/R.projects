library(tidyverse)
library(lubridate)
library(scales)
library(magrittr)
library(forcats)
library(ggrepel)
library(stringi)
library(shiny)
library(formattable)
library(odbc)
# library(RSQLServer)
# library(DBI)
library(RODBC)
# library(RODBCDBI)
# library(RPostgreSQL)
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

library(RODBC) 
dbhandle <- odbcDriverConnect('driver={SQL Server};
                              server=178.64.175.38;
                              database=1c_akto_test;
                              uid=outdatetest;
                              pwd=q1w2cx986po')
res <- sqlQuery(dbhandle, 'select * from information_schema.tables')
# dbListFields(dbhandle, "exchange_akto")
# df <- dbReadTable(dbhandle, "exchange_akto") %>%
df <- sqlQuery(dbhandle, "select * from exchange_akto") %>%
  as_tibble()#  %>%                
                
stop()                
# סלמענטל הנאיגונא
odbc::odbcListDrivers()

con <- DBI::dbConnect(# odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "178.64.175.38",
                      Database = "1c_akto_test",
                      UID      = "outdatetest",
                      PWD      = "q1w2cx986po",
                      encoding = "UTF-8",
                      Port     = 1433)

# t <- dbListTables(con)
dbListFields(con, "exchange_akto")
df <- dbReadTable(con, "exchange_akto") %>%
  as_tibble()#  %>%
  # mutate_if(is.character, `Encoding<-`, "UTF-8")


# stri_enc_detect(df$name)
m <- stri_conv(df$name, from="UTF-8", to="windows-1251", to_raw=FALSE)
  
stop()
# formattable samples ===============
# https://cran.r-project.org/web/packages/formattable/vignettes/formattable-data-frame.html
products <- data.frame(id = 1:5, 
                       price = c(10, 15, 12, 8, 9),
                       rating = c(5, 4, 4, 3, 4),
                       market_share = percent(c(0.1, 0.12, 0.05, 0.03, 0.14)),
                       revenue = accounting(c(55000, 36400, 12000, -25000, 98100)),
                       profit = accounting(c(25300, 11500, -8200, -46000, 65000)))
products

sign_formatter <- formatter("span", 
                            style = x ~ style(color = ifelse(x > 0, "green", 
                                                             ifelse(x < 0, "red", "black"))))
m <- formattable(products, list(profit = sign_formatter))
dput(products)
