<<<<<<< HEAD:42 IoT_test/sensor_on_disk.R
# rm(list=ls()) # очистим все переменные
=======
rm(list=ls()) # Г®Г·ГЁГ±ГІГЁГ¬ ГўГ±ГҐ ГЇГҐГ°ГҐГ¬ГҐГ­Г­Г»ГҐ
>>>>>>> origin/master:43-IoT_field/sensor_on_disk.R

library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)

options(warn=1) # http://stackoverflow.com/questions/11239428/how-to-change-warn-setting-in-r

source("disk_funcs.R")

# tidyr 0.4.0 Nested data frames
# http://blog.rstudio.org/2016/02/02/tidyr-0-4-0/

# ГѓГҐГ­ГҐГ°ГЁГ¬ ГІГ®Г·ГЄГЁ ГўГ­ГіГІГ°ГЁ Г®ГЄГ°ГіГ¦Г­Г®Г±ГІГЁ ГҐГ¤ГЁГ­ГЁГ·Г­Г®ГЈГ® Г°Г Г¤ГЁГіГ±Г .
# Г‘Г·ГЁГІГ ГҐГ¬, Г·ГІГ® ГўГ±ГҐ Г·Г Г±ГІГЁГ¶Г» ГҐГ¤ГЁГ­ГЁГ·Г­Г®ГЈГ® Г§Г Г°ГїГ¤Г , ГЇГ®ГЅГІГ®Г¬Гі ГҐГЈГ® Г®ГЇГіГ±ГЄГ ГҐГ¬
# df.particles <- data.frame(x = c(0.1, 0.2, 0.3), y = c(0.3, 0.4, 0.5), fixed = c(FALSE, FALSE, FALSE))
df.particles <- data.frame(j = (1:4)) %>%
  mutate(angle = runif(n(), min=0, max=2*pi), r = runif(n(), min=0, max=1)) %>%
  mutate(x = r*cos(angle), y = r*sin(angle), fixed = FALSE) %>%
  select(-angle, -r, -j)

# lapply(particles, function(x){print(paste0("-", x, "-"))})

# Г¤Г«Гї Г±ГµГ®Г¤ГЁГ¬Г®Г±ГІГЁ Г§Г Г¤Г Г·ГЁ ГЈГҐГ­ГҐГ°ГЁГ°ГіГҐГ¬ ГІГ ГЄГ¦ГҐ Г§Г ГґГЁГЄГ±ГЁГ°Г®ГўГ Г­Г­Г»ГҐ ГІГ®Г·ГЄГЁ Г­Г  Г®ГЄГ°ГіГ¦Г­Г®Г±ГІГЁ
perimeter.particles <- data.frame(j = (1:40)) %>%
  mutate(angle = (j-1)*(2*pi/n()), x = 1.3*cos(angle), y = 1.3*sin(angle), fixed = TRUE) %>%
  select(-angle, -j)


df.particles %<>% dplyr::bind_rows(perimeter.particles) %>%
  mutate(force.x = 0, force.y = 0, j = seq_len(n()))
  



# =======================================
dfs <- df.particles

gp <- disk_plot(dfs)
print(gp)

start.time <- Sys.time()
max.force <- 10
step <- 0

while (max.force > 0.5) {
  # ГЇГ°Г®ГўГҐГ°ГїГҐГ¬, Г·ГІГ® ГІГ®Г·ГЄГЁ Г­ГҐ ГіГёГ«ГЁ ГЁГ§ Г®ГЄГ°ГіГ¦Г­Г®Г±ГІГЁ ГҐГ¤ГЁГ­ГЁГ·Г­Г®ГЈГ® Г°Г Г¤ГЁГіГ±Г 
  if(nrow(filter(dfs, !fixed, x^2+y^2 > 1.2)) > 0) break
  
  # ГЇГ°Г®ГўГҐГ¤ГҐГ¬ Г°Г Г±Г·ГҐГІ Г±ГЁГ« ГЁ ГЇГ°ГЁГ­ГіГ¤ГЁГІГҐГ«ГјГ­Г® Г§Г Г­ГіГ«ГЁГ¬ ГўГ±ГҐ Г±ГЁГ«Г», Г¤ГҐГ©Г±ГІГўГіГѕГ№ГЁГҐ Г­Г  ГІГ®Г·ГЄГЁ Г­Г  Г®ГЄГ°ГіГ¦Г­Г®Г±ГІГЁ
  dfs <- calc_qstep(dfs) %>%
    mutate(force.x = force.x * !fixed, force.y = force.y * !fixed)
  
  # gp <- disk_plot(dfs)
  # print(gp)
  
  # Г®ГЇГ°ГҐГ¤ГҐГ«ГЁГ¬ Г¬Г ГЄГ±ГЁГ¬Г Г«ГјГ­ГіГѕ Г±ГЁГ«Гі, Г¤ГҐГ©Г±ГІГўГіГѕГ№ГіГѕ Г­Г  Г·Г Г±ГІГЁГ¶Гі
  max.force <- max(sqrt(dfs$force.x ^ 2 + dfs$force.y ^ 2))
  
  # ГЇГ°Г®ГўГ®Г¤ГЁГ¬ Г±Г¬ГҐГ№ГҐГ­ГЁГҐ ГІГ®Г·ГҐГЄ
  dfs %<>%
    mutate(x.old = x, y.old = y) %>%
    mutate(x = x + force.x / 1e4, y = y + force.y / 1e4)
  
  step <- step + 1
  print(paste0(
    "iteration #",
    step,
    " ГђГ Г±Г·ГҐГІ Г¤Г«ГЁГІГ±Гї ",
    round(as.numeric(difftime(Sys.time(), start.time, unit = "sec")), digits = 0),
    # round(Sys.time() - start.time, digits = 0),
    " Г±ГҐГЄ, max force = ",
    max.force
  ))
}

filter(dfs, !fixed, x^2+y^2 > 1)

gp <- disk_plot(dfs)
print(gp)

# сохраняем объект
saveRDS(dfs, "sensors_calulated.rds") #, compress = FALSE, ascii = TRUE)

print("End...")
stop()

<<<<<<< HEAD:42 IoT_test/sensor_on_disk.R
# =========================== мапируем координаты на  GIS ==========================================
# попробуем оптимизировать маршрут обхода
# 1. Убираем фиксированные точки
df.calc <- filter(dfs, !fixed) %>% mutate (r2 = sqrt(x^2+y^2))
# 2. Выбираем в качестве начальной точки датчик, максимально близко расположенный к краю поля
n1 <- filter(df.calc, r2 == max(r2))[['j']]

# теперь проводим симуляцию различных вариантов расстановки сенсоров
# получаем последовательность номеров и убираем n1, его будем принудительно ставить первым
seqsensor <- df.calc$j %>% .[. != n1]

# сгенерируем список произвольных последовательностей обхода сенсоров, начинающуюся с n1
routes <- lapply(1:10, function(x){ c(n1, sample(seqsensor, length(seqsensor), replace = FALSE)) })

# расчет длины маршрута обхода по заданной последовательности
calc_path_length <- function(path){
  # path -- последовательность обхода, номера сенсоров
  df.points <- data.frame(l = head(path, -1), r = tail(path, -1)) %>%
    mutate(s = sqrt((dfs$x[l]-dfs$x[r])^2 + (dfs$y[l]- dfs$y[r])^2))
  
  # for (i in 1:lenght(path)-1){
  #print(df.points)
  # invisible(readline(prompt="Press [enter] to continue"))
  sum(df.points$s)
}

t <- unlist(lapply(routes, calc_path_length))
=======
# =========================== Г¬Г ГЇГЁГ°ГіГҐГ¬ ГЄГ®Г®Г°Г¤ГЁГ­Г ГІГ» Г­Г   GIS ==========================================
# ГЇГ®ГЇГ°Г®ГЎГіГҐГ¬ Г®ГЇГІГЁГ¬ГЁГ§ГЁГ°Г®ГўГ ГІГј Г¬Г Г°ГёГ°ГіГІ Г®ГЎГµГ®Г¤Г 
# 1. Г“ГЎГЁГ°Г ГҐГ¬ ГґГЁГЄГ±ГЁГ°Г®ГўГ Г­Г­Г»ГҐ ГІГ®Г·ГЄГЁ
df.calc <- filter(dfs, !fixed)
# 2. Г‚Г»ГЎГЁГ°Г ГҐГ¬ Гў ГЄГ Г·ГҐГ±ГІГўГҐ Г­Г Г·Г Г«ГјГ­Г®Г© ГІГ®Г·ГЄГЁ Г¤Г ГІГ·ГЁГЄ, Г¬Г ГЄГ±ГЁГ¬Г Г«ГјГ­Г® ГЎГ«ГЁГ§ГЄГ® Г°Г Г±ГЇГ®Г«Г®Г¦ГҐГ­Г­Г»Г© ГЄ ГЄГ°Г Гѕ ГЇГ®Г«Гї
n1 <- filter(df.calc, x == max(x))
>>>>>>> origin/master:43-IoT_field/sensor_on_disk.R

stop()


# =========================== Г¬Г ГЇГЁГ°ГіГҐГ¬ ГЄГ®Г®Г°Г¤ГЁГ­Г ГІГ» Г­Г   GIS ==========================================
# Г±ГўГїГ§Гј ГЅГ«Г«ГЁГЇГ±Г  Г± Г®ГЄГ°ГіГ¦Г­Г®Г±ГІГјГѕ
# http://stu.sernam.ru/book_ehm.php?id=38
# http://www.mathprofi.ru/linii_vtorogo_poryadka_ellips_i_okruzhnost.html
# http://math.stackexchange.com/questions/597090/computing-a-matrix-to-convert-an-x-y-point-on-an-ellipse-to-a-circle

# write.csv(dfs, file="sensors_calc.csv")
# https://yandex.ru/maps/?text=54.860510,38.655286&z=5
# ГЏГ®Г«ГіГ°ГїГ¤ГЁГ­ГЄГЁ
x0.geo <- 38.655286 # Longitude
y0.geo <- 54.860510 # Latitude
rx.geo <- sqrt((x0.geo - 38.662979)^2 + (y0.geo - 54.860416)^2) # ГЇГ°Г ГўГ Гї ГЈГ®Г°ГЁГ§Г®Г­ГІГ Г«ГјГ­Г Гї ГІГ®Г·ГЄГ 
ry.geo <- sqrt((x0.geo - 38.655267)^2 + (y0.geo - 54.865068)^2) # ГўГҐГ°ГµГ­ГїГї ГўГҐГ°ГІГЁГЄГ Г«ГјГ­Г Гї ГІГ®Г·ГЄГ 

# Г‡Г®Г®Г«Г®ГЈГЁГ·ГҐГ±ГЄГ Гї 2
x0.geo <- 37.58022 # Longitude
y0.geo <- 55.76559 # Latitude
rx.geo <- sqrt((0.01)^2) # ГЇГ°Г ГўГ Гї ГЈГ®Г°ГЁГ§Г®Г­ГІГ Г«ГјГ­Г Гї ГІГ®Г·ГЄГ 
ry.geo <- sqrt((0.01)^2) # ГўГҐГ°ГµГ­ГїГї ГўГҐГ°ГІГЁГЄГ Г«ГјГ­Г Гї ГІГ®Г·ГЄГ 


# Г  ГҐГ№ГҐ Г­ГҐ Г§Г ГЎГ»ГўГ ГҐГ¬, Г·ГІГ® Г­Г Г¤Г® ГЇГ°ГҐГ®ГЎГ°Г Г§Г®ГўГ»ГўГ ГІГј ГЄГ®Г®Г°Г¤ГЁГ­Г ГІГ» ГЁГ§ Г¬ГҐГІГ°Г®Гў Гў ГЈГ°Г Г¤ГіГ±Г»
# Г‚Г»Г·ГЁГ±Г«ГҐГ­ГЁГҐ Г°Г Г±Г±ГІГ®ГїГ­ГЁГї ГЁ Г­Г Г·Г Г«ГјГ­Г®ГЈГ® Г Г§ГЁГ¬ГіГІГ  Г¬ГҐГ¦Г¤Гі Г¤ГўГіГ¬Гї ГІГ®Г·ГЄГ Г¬ГЁ Г­Г  Г±ГґГҐГ°ГҐ
# http://gis-lab.info/qa/great-circles.html



df.geo <- dfs %>%
  mutate(x = x * rx.geo + x0.geo, y = y * ry.geo + y0.geo)

# http://www.gpsvisualizer.com/convert_input
write.table(
  df.geo %>% select(j, x, y) %>% rename(lon = x, lat = y, num = j),
  file = "sensors_calc.csv",
  sep = ",",
  row.names = FALSE,
  qmethod = "double"
)


stop()
# ===============================================================================================
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


# Г±Г·ГЁГІГ ГҐГ¬ Г¤Г«Гї Г·Г Г±ГІГЁГ¶Г» Г±ГЁГ«Гі ГўГ®Г§Г¤ГҐГ©Г±ГІГўГЁГї Г®Г±ГІГ Г«ГјГ­Г»Гµ Г·Г Г±ГІГЁГ¶, ГЇГ°ГҐГ¤Г®Г±ГІГ ГўГ«ГҐГ­Г­Г»Гµ Г±ГЇГЁГ±ГЄГ®Г¬
calc_force <- function(particle, plist){
  # plist -- Г±ГЇГЁГ±Г®ГЄ Г·Г Г±ГІГЁГ¶
  # print(paste0("(", particle[[1]], ", ", particle[[2]], ") <-> ", plist))
  
  # ГЁГ±ГЇГ®Г«ГјГ§ГіГҐГ¬ ГўГҐГЄГІГ®Г°ГЁГ§Г Г¶ГЁГѕ ГґГіГ­ГЄГ¶ГЁГ© ГЁ Г±ГўГ®Г©Г±ГІГўГ® Г¤Г®ГЇГ®Г«Г­ГҐГ­ГЁГї
  #dd <- lapply(plist, function(x){})
  #dd <- plist - particle
  #print(dd)
} 

# http://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
t <- lapply(particles, calc_force, plist = particles)
