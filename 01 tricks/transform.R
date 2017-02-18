# Transposing in dplyr ==============================
# http://stackoverflow.com/questions/34004008/transposing-in-dplyr


df <- structure(
  list(HEADER = c("HOME_TRPM", "AWAY_TRPM", "HOME_TEAM","AWAY_TEAM"), 
       price = c("0.863104076023855", "-0.845186446996287","CHA", "NOP")), 
  .Names = c("HEADER", "price"), row.names = c(NA, 4L), class = "data.frame")


library(dplyr)
library(magrittr)
library(tidyr)

res <- df %>% spread(HEADER, price)
