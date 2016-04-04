rm(list=ls()) # очистим все переменные

library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)

source("disk_funcs.R")

# tidyr 0.4.0 Nested data frames
# http://blog.rstudio.org/2016/02/02/tidyr-0-4-0/

# Генерим точки внутри окружности единичного радиуса.
# Считаем, что все частицы единичного заряда, поэтому его опускаем
# df.particles <- data.frame(x = c(0.1, 0.2, 0.3), y = c(0.3, 0.4, 0.5), fixed = c(FALSE, FALSE, FALSE))
df.particles <- data.frame(point = (1:13)) %>%
  mutate(angle = runif(n(), min=0, max=2*pi), r = runif(n(), min=0, max=1)) %>%
  mutate(x = r*cos(angle), y = r*sin(angle), fixed = FALSE) %>%
  select(-angle, -point, -r)

# lapply(particles, function(x){print(paste0("-", x, "-"))})

# для сходимости задачи генерируем также зафиксированные точки на окружности
perimeter.particles <- data.frame(point = (0:20)) %>%
  mutate(angle = point*(2*pi/n()), x = 1.3*cos(angle), y = 1.3*sin(angle), fixed = TRUE) %>%
  select(-angle, -point)


df.particles %<>% dplyr::bind_rows(perimeter.particles) %>%
  mutate(force.x = 0, force.y = 0)


# =======================================
dfs <- df.particles

gp <- disk_plot(dfs)
print(gp)

max.force <- 10
step <- 0

while (max.force > 0.05){
  # проведем расчет сил и занулим все силы, действующие на точки на окружности
  dfs <- calc_qstep(dfs) %>%
    mutate(force.x = force.x * !fixed, force.y = force.y * !fixed)
  
  # gp <- disk_plot(dfs)
  # print(gp)

  # определим максимальную силу, действующую на частицу
  max.force <- max(sqrt(dfs$force.x^2+dfs$force.y^2))
    
  # проводим смещение точек
  dfs %<>%
    mutate(x.old = x, y.old = y) %>%
    mutate(x = x + force.x/1e3, y = y + force.y/1e3)
  
  step <- step + 1
  print(paste0("step #", step, " max force = ", max.force))
}

gp <- disk_plot(dfs)
print(gp)


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