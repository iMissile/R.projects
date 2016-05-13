# генерируем тестовые файлы для отладки интерфейса
#library(tidyr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(dplyr)
library(readr)
library(jsonlite)
library(magrittr)
library(curl)
library(httr)
library(ggthemes)
library(ggdendro) # для пустой темы
#library(ggmap)
library(RColorBrewer) # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
library(scales)
library(gtable)
library(grid) # для grid.newpage()
library(gridExtra) # для grid.arrange()

# library(KernSmooth)
#library(akima)
#library(rdrop2)
# library(rgl)


url <- "api.openweathermap.org/data/2.5/"   
MoscowID <- '524901'
APPID <- '19deaa2837b6ae0e41e4a140329a1809'
resp <- GET(paste0(url, "weather?id=", MoscowID, "&APPID=", APPID))
if(status_code(resp) == 200){
  r <- content(resp)
  # конструируем вектор
  d <- data.frame(
    # timestamp = now(),
    timestamp = as.POSIXct(r$dt, origin='1970-01-01'),
    temp = round(r$main$temp - 273, 1), # пересчитываем из кельвинов в градусы цельсия
    pressure = round(r$main$pressure * 0.75006375541921, 0), # пересчитываем из гектопаскалей (hPa) в мм рт. столба
    humidity = round(r$main$humidity, 0)
    # precipitation = r$main$precipitation
  )
}

df <- data.frame(x = c(0, 1), y = c(0, 1))

windowsFonts(verdana = "TT Verdana")
windowsFonts(geinspira = "GE Inspira")
windowsFonts(corbel = "Corbel")
p <- ggplot(df, aes(x, y)) + 
  geom_point() +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1), fill = "peachpuff") +
  geom_text(aes(.5, .8), label = paste0(d$temp, " C"), size = rel(40), color="blue", family = "verdana") +
  geom_text(aes(.5, .5), label = paste0(d$pressure, " мм"), size = 16, color="blue", family = "verdana") +
  geom_text(aes(.5, .3), label = paste0(d$humidity, " %"), size = 16, color="blue", family = "verdana") +
  geom_text(aes(.5, .1), label = paste0(d$timestamp), size = rel(8), color="blue", family = "verdana") +
  theme_dendro() # совершенно пустая тема


# print(p)



# теперь делаем модификацию с автомасштабированием ===================================


# grobframe <- arrangeGrob(p,
#                          main = textGrob("\nArrangeGrob Test", gp = gpar(fontsize=18, fontface="bold.italic", fontsize=18)),
#                          sub = textGrob("*subtitle location*", x=0, hjust=-0.5, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 15)))
# 
# print(grobframe)


# autoresize ==============================================
# взят отсюда: https://ryouready.wordpress.com/2012/08/01/creating-a-text-grob-that-automatically-adjusts-to-viewport-size/
gr <- NA
resizingTextGrob <- function(..., max.font.size = 40) {
  gr <- grob(tg = textGrob(...), cl = "resizingTextGrob")
  # добавим свой доп. атрибут -- максимальный размер текста до которого масштабируем
  # str(gr)
  gr[['max.font.size']] <- max.font.size
  gr
}
drawDetails.resizingTextGrob <- function(x, recording = TRUE) { grid.draw(x$tg) }
preDrawDetails.resizingTextGrob <- function(x)
{
  h <- convertHeight(unit(1, "snpc"), "mm", valueOnly = TRUE)
  fs <- rescale(h, to = c(x$max.font.size, 7), from = c(50, 5))
  print(paste0("h = ", h, ", fs = ", fs))
  # browser()
  #pushViewport(viewport(gp = gpar(fontsize = fs, fontface = 'bold')))
  pushViewport(viewport(gp = gpar(fontsize = fs)))
}
postDrawDetails.resizingTextGrob <- function(x) { popViewport() }
# ============================================================================


# from here: https://github.com/hadley/ggplot2/wiki/Mixing-ggplot2-graphs-with-other-graphical-output
p <- qplot(0:10, 0:10) + theme_bw()
g <- ggplotGrob(qplot(1, 1))
g1 <- textGrob(label = "Static sizeText")
g2 <- resizingTextGrob(label = "Resizing Text", max.font.size = 16)
p + 
  annotation_custom(grob = g1, xmin = 2, xmax = 2, ymin = 6) +
  annotation_custom(grob = g2, xmin = 2, ymin = 4)

# тест работает ==============================================================
l1 <- resizingTextGrob(label = paste0(d$temp, " C"), max.font.size = 120)
l2 <- resizingTextGrob(label = paste0(d$pressure, " мм"), max.font.size = 80)
l3 <- resizingTextGrob(label = paste0(d$humidity, " %"), max.font.size = 80)
l4 <- resizingTextGrob(label = paste0(d$timestamp), max.font.size = 60)

p <- ggplot(df, aes(x, y)) + 
  geom_point() +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1), fill = "peachpuff") +
  # geom_text(aes(.5, .8), label = paste0(d$temp, " C"), size = rel(40), color="blue", family = "verdana") +
  annotation_custom(grob = l1, xmin = 0.3, xmax = 0.7, ymin = .7, ymax = .9) +
  annotation_custom(grob = l2, xmin = 0.3, xmax = 0.7, ymin = .5, ymax = .7) +
  annotation_custom(grob = l3, xmin = 0.3, xmax = 0.7, ymin = .3, ymax = .5) +
  annotation_custom(grob = l4, xmin = 0.3, xmax = 0.7, ymin = .1, ymax = .3) +
  #geom_text(aes(.5, .5), label = paste0(d$pressure, " мм"), size = 16, color="blue", family = "verdana") +
  #geom_text(aes(.5, .3), label = paste0(d$humidity, " %"), size = 16, color="blue", family = "verdana") +
  #geom_text(aes(.5, .1), label = paste0(d$timestamp), size = rel(8), color="blue", family = "verdana") +
  theme_dendro() # совершенно пустая тема


p