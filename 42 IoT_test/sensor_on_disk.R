rm(list=ls()) # очистим все переменные

library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)


# tidyr 0.4.0 Nested data frames
# http://blog.rstudio.org/2016/02/02/tidyr-0-4-0/

# Генерим точки внутри окружности единичного радиуса.
# Считаем, что все частицы единичного заряда, поэтому его опускаем

df.particles = data.frame(x = c(0.1, 0.2, 0.3), y = c(0.3, 0.4, 0.5), fixed = c (0, 0, 0))
# lapply(particles, function(x){print(paste0("-", x, "-"))})


# после всех экспериментов пришел к заключению, что для моей задачи скорость не так важна, как понимание процедур
# вычисления не векторизуются, поэтому оставляю простой for
# можно было делать список из частиц с их характеристиками, но фрейм проще охватить взглядом
# http://stackoverflow.com/questions/3337533/in-r-how-do-you-loop-over-the-rows-of-a-data-frame-really-fast

df <- df.particles
for (i in 1:nrow(df)){
  x0 <- df$x[i]
  y0 <- df$y[i]
  # считаем проекцию и растояния относительно текущей точки
  df.step <- df %>%
    mutate(dx = x - x0, dy = y - y0) %>%
    mutate(r2 = dx^2+dy^2) %>%
    # исключаем строки с нулевым расстоянием, не забываем при этом про наличие машинного нуля
    filter(r2 > 1e-20) %>%
    mutate(force.x0 = -dx/sqrt(r2)*(1/r2)) %>%
    mutate(force.y0 = -dy/sqrt(r2)*(1/r2))
    # необходимо убрать NaN в столбцах force, можно делать это напрямую
    # http://stackoverflow.com/questions/34057579/removing-nan-using-dplyr
    # а можно это делать и раньше, исключая строки с нулевым расстоянием
  
  
  # df1 <- df.step %>%
  #  summarise(force.x = sum(force.x0), force.y = sum(force.y0))
  
  # сумма векторов даст результирующее воздействие на частицу, которое и необходимо выдать наружу
  df$force.x[i] <- sum(df.step$force.x0)
  df$force.y[i] <- sum(df.step$force.y0)
  
  print(paste0("step #", i))
  print(paste0("(", x0, ", ", y0, ") <-> force (", df1$force.x, ", ", df1$force.y, ")"))
  
}

# рисование окружности на ggplot
# http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleFun <- function(center = c(0,0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

disk_plot <- function(df) {
  # отрисуем расположение точек и действующие силы
  # Cookbook по работе с графикой, общий заход: http://www.cookbook-r.com/Graphs/
  # радиус = 1, координаты [-1; 1] по обеим осям
  
  circdat <- circleFun(c(0,0), 2, npoints = 100)

  gp <- ggplot(data = df, aes(x = x, y = y)) +
    theme_bw() +
    theme(
      # text = element_text(size = 20), #, family = "verdana"),
      # axis.text.x = element_text(vjust = 3),
      # axis.text.y = element_text(hjust=3),
      # axis.title.x = element_text(angle = 0, hjust = .5, vjust = 0, face = "plain"),
      # axis.title.y = element_text(angle = 90, hjust = .5, vjust = 2, face = "plain")
    ) +
    # управление формой и цветом
    # http://www.cookbook-r.com/Graphs/Shapes_and_line_types/
    # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
    # scale_colour_brewer(palette = "Dark2", name  = paste(exp.env, "\nangle = ", val, "\n", sep = "")) +
    #geom_line(size = 1, color = RColorBrewer::brewer.pal(3, "Set2")[1]) +
    geom_point(size = 2, fill = "black", shape = 21) +    # White fill
    # рисуем векторное поле
    # http://stackoverflow.com/questions/14936504/vector-field-visualisation-r
    # http://blog.revolutionanalytics.com/2013/02/a-review-of-the-r-graphics-cookbook.html
    geom_segment(aes(xend = x + force.x/600, yend = y + force.y/600), colour = "red", 
                 arrow = arrow(length = unit(0.2,"cm")), size = 0.6) + 
    
    # рисуем окружность
    # http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    geom_path(data = circdat, aes(x, y)) + # geom_path will do open circles, geom_polygon will do filled circles
  
    # geom_line(size = 1.5, aes(color = type)) +
    # http://ggplot.yhathq.com/docs/xlim.html
    xlim(-1, 1) +
    ylim(-1, 1) +
    
    # http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
    # This is done by giving a formula to facet_grid(), of the form vertical ~ horizontal.
    # насчет масштабирования осей см http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/#free-scales
    #facet_grid(. ~ type, scales = "free_y") +
    labs(x = "Ось X", y = "Ось Y")
  
  gp
}




disk_plot(df)


print("End...")
stop()

# http://stackoverflow.com/questions/15059076/call-apply-like-function-on-each-row-of-dataframe-with-multiple-arguments-from-e
myf <- function(x0, y0, plist){
  t <- plist %>%
    mutate(dx = x0 - x, dy = y0 - y)
  # print(paste0("(", x, ", ", y, ") <-> ", str(plist)))
  print(t)
}

pp <- particles %>%
  mutate(value = myf(x, y, .))


stop()

particles = list(list(x=1, y=2), list(x=3, y=4), list(x=5, y=6))
lapply(particles, function(x){print(paste0("=", x, "="))})


# считаем для частицы силу воздействия остальных частиц, предоставленных списком
calc_force <- function(particle, plist){
  # plist -- список частиц
  # print(paste0("(", particle[[1]], ", ", particle[[2]], ") <-> ", plist))
  
  # используем векторизацию функций и свойство дополнения
  #dd <- lapply(plist, function(x){})
  #dd <- plist - particle
  #print(dd)
} 

# http://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
t <- lapply(particles, calc_force, plist = particles)