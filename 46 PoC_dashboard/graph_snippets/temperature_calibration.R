# калибровка данных аналоговых температурных датчиков, интегрированных с датчиками влажности почвы
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(ggdendro) # для пустой темы
library(lubridate) #load second!
library(dplyr)
library(readr)
library(jsonlite)
library(magrittr)
library(httr)
library(ggthemes)
#library(ggmap)
library(RColorBrewer) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
library(scales)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()
library(curl)
# library(KernSmooth)
#library(akima)
#library(rdrop2)
# library(rgl)

# эксперимент №1 ------------------------------------------------------------
df1 <- data.frame(humidity = c(0, 0.2, 0.25, 0.4, 0.6),
                 measurement = c(2578944, 2545344, 1646496, 959744, 953440))

p1 <- ggplot(df1, aes(humidity, measurement)) +
  geom_line() +
  geom_point(shape = 1) +
  geom_smooth()

p1


# эксперимент №2 ------------------------------------------------------------
df2 <- data.frame(humidity = c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4),
                 measurement = c(2375232, 2443488, 2441392, 2176768, 1219120, 1375568, 1163456, 947152))

p2 <- ggplot(df2, aes(humidity, measurement)) +
  geom_line() +
  geom_point(shape = 1) +
  geom_smooth()

p2
