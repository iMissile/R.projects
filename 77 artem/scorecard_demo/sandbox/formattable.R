library(tidyverse)
library(lubridate)
library(scales)
library(magrittr)
library(forcats)
library(ggrepel)
library(stringi)
library(shiny)
library(formattable)

# formattable samples ===============
# https://cran.r-project.org/web/packages/formattable/vignettes/formattable-data-frame.html
# https://renkun.me/formattable/

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
formattable::normalize(products$rating, min=2, max=6)
b <- 3
e <- 7


products$rating %>% formattable::normalize(min=(min(.)-b)/(e-b), max=(max(.)-b)/(e-b))

norm1 <- function(x, left, right){
  range <- (c(min(x), max(x))-left)/(right-left)
  formattable::normalize(x, min=range[1], max=range[2])
}

formattable(products, list(profit=sign_formatter, 
                           area(col=c("rating", "price")) ~ color_bar("lightblue", function(x) norm1(x, 3, 7))))


# dput(products)
