library(dplyr)
library(magrittr)

# tidyr 0.4.0 Nested data frames
# http://blog.rstudio.org/2016/02/02/tidyr-0-4-0/

# √енерим точки внутри окружности единичного радиуса.
# —читаем, что все частицы единичного зар€да, поэтому его опускаем

df.particles = data.frame(x = c(1, 2, 3), y = c(7, 8, 9))
# lapply(particles, function(x){print(paste0("-", x, "-"))})


# после всех экспериментов пришел к заключению, что дл€ моей задачи скорость не так важна, как понимание процедур
# вычислени€ не векторизуютс€, поэтому оставл€ю простой for
# можно было делать список из частиц с их характеристиками, но фрейм проще охватить взгл€дом
# http://stackoverflow.com/questions/3337533/in-r-how-do-you-loop-over-the-rows-of-a-data-frame-really-fast

df <- df.particles
for (i in 1:nrow(df)){
  x0 <- df$x[i]
  y0 <- df$y[i]
  # считаем проекцию и расто€ни€ относительно текущей точки
  df.step <- df %>%
    mutate(dx = x - x0, dy = y - y0) %>%
    mutate(r2 = dx^2+dy^2) %>%
    # исключаем строки с нулевым рассто€нием, не забываем при этом про наличие машинного нул€
    filter(r2 > 1e-20) %>%
    mutate(force.x0 = -dx/sqrt(r2)*(1/r2)) %>%
    mutate(force.y0 = -dy/sqrt(r2)*(1/r2))
    # необходимо убрать NaN в столбцах force, можно делать это напр€мую
    # http://stackoverflow.com/questions/34057579/removing-nan-using-dplyr
    # а можно это делать и раньше, исключа€ строки с нулевым рассто€нием
  
  
  df1 <- df.step %>%
    summarise(force.x = sum(force.x0), force.y = sum(force.y0))
  
  # сумма векторов даст результирующее воздействие на частицу, которое и необходимо выдать наружу
  
  print(paste0("step #", i))
  print(paste0("(", x0, ", ", y0, ") <-> "))
}

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


# считаем дл€ частицы силу воздействи€ остальных частиц, предоставленных списком
calc_force <- function(particle, plist){
  # plist -- список частиц
  # print(paste0("(", particle[[1]], ", ", particle[[2]], ") <-> ", plist))
  
  # используем векторизацию функций и свойство дополнени€
  #dd <- lapply(plist, function(x){})
  #dd <- plist - particle
  #print(dd)
} 

# http://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
t <- lapply(particles, calc_force, plist = particles)