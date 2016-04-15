library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)


# после всех экспериментов пришел к заключению, что для моей задачи скорость не так важна, как понимание процедур
# вычисления не векторизуются, поэтому оставляю простой for
# можно было делать список из частиц с их характеристиками, но фрейм проще охватить взглядом
# http://stackoverflow.com/questions/3337533/in-r-how-do-you-loop-over-the-rows-of-a-data-frame-really-fast

calc_qstep <- function(df) {
  for (i in 1:nrow(df)) {
    # для укорения расчетов исключаем расчет сил для "прибитых" точек
    if (df$fixed[i]) next # http://www.endmemo.com/program/R/forloop.php
    
    x0 <- df$x[i]
    y0 <- df$y[i]
    # считаем проекцию и растояния относительно текущей точки
    df.step <- df %>%
      mutate(dx = x - x0, dy = y - y0) %>%
      mutate(r2 = dx^2 + dy^2) %>%
      # исключаем строки с нулевым расстоянием, не забываем при этом про наличие машинного нуля
      filter(r2 > 1e-20) %>%
      mutate(force.x0 = -dx / sqrt(r2) * (1/r2)) %>%
      mutate(force.y0 = -dy / sqrt(r2) * (1/r2))
    # необходимо убрать NaN в столбцах force, можно делать это напрямую
    # http://stackoverflow.com/questions/34057579/removing-nan-using-dplyr
    # а можно это делать и раньше, исключая строки с нулевым расстоянием
    
    
    # df1 <- df.step %>%
    #  summarise(force.x = sum(force.x0), force.y = sum(force.y0))
    
    # сумма векторов даст результирующее воздействие на частицу, которое и необходимо выдать наружу
    df$force.x[i] <- sum(df.step$force.x0)
    df$force.y[i] <- sum(df.step$force.y0)
    
    ## print(paste0("point #", i))
    ## print(paste0("(", x0, ", ", y0, ") <-> force (", df$force.x[i], ", ", df$force.y[i], ")"))
  }
  return(df)
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
  
  circdat <- circleFun(c(0, 0), 2, npoints = 100)
  
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
    geom_text(aes(label = j), hjust = 0.5, vjust = -1) +
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
    xlim(-1.5, 1.5) +
    ylim(-1.5, 1.5) +
    
    # http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/
    # This is done by giving a formula to facet_grid(), of the form vertical ~ horizontal.
    # насчет масштабирования осей см http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/#free-scales
    #facet_grid(. ~ type, scales = "free_y") +
    labs(x = "Ось X", y = "Ось Y")
  
  gp
}

# расчет длины маршрута обхода по заданной последовательности
calc_path_length <- function(df.sensor, path){
  # path -- последовательность обхода, номера сенсоров
  df.points <- data.frame(l = head(path, -1), r = tail(path, -1)) %>%
    mutate(s = sqrt((df.sensor$x[l]-df.sensor$x[r])^2 + (df.sensor$y[l]- df.sensor$y[r])^2))
  
  # for (i in 1:length(path)-1){
  #print(df.points)
  # invisible(readline(prompt="Press [enter] to continue"))
  sum(df.points$s)
}

optimize_path <- function(dfs) {
  # попробуем оптимизировать маршрут обхода
  # 1. Убираем фиксированные точки
  df.calc <- filter(dfs, !fixed) %>% mutate (r = sqrt(x^2+y^2))
  # 2. Выбираем в качестве начальной точки датчик, максимально близко расположенный к краю поля
  n1 <- filter(df.calc, r == max(r))[['j']]
  
  # теперь проводим симуляцию различных вариантов расстановки сенсоров
  # получаем последовательность номеров и убираем n1, его будем принудительно ставить первым
  seqsensor <- df.calc$j %>% .[. != n1]
  
  # сгенерируем список произвольных последовательностей обхода сенсоров, начинающуюся с n1, 
  # и оставим только уникальные
  routes <- unique(lapply(1:5000000, 
                          function(x){ c(n1, sample(seqsensor, length(seqsensor), replace = FALSE)) }
                          ))
  
  print(paste0("Уникальных путей обхода: ", length(routes)))
  
  min.length <- NA
  for (cur.path in routes){
    cur.length <- calc_path_length(df.sensor = dfs, path = cur.path) 
    
    if (is.na(min.length) || cur.length < min.length){
      best.route <- list(s = cur.length, path = cur.path)
      min.length <- cur.length
      print(cur.length)
    }
  }
  
  # лучший маршрут -- best.route
  # пересортируем сенсоры в исходной таблице в соотв. с этой последовательностью
  # объединяем лучший путь и делаем arrange
  
  path.frame <- data.frame(j = best.route$path, j.path = 1:length(best.route$path))
  
  df.best <- df.calc %>%
    left_join(path.frame, by = "j") %>%
    arrange(j.path)
  
  print(paste0("Minimal route length: ", min.length))
  
  df.best
}
