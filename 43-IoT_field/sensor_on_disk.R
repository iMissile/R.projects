rm(list=ls()) # î÷èñòèì âñå ïåðåìåííûå

library(dplyr)
library(magrittr)
library(ggplot2) #load first! (Wickham)
library(lubridate) #load second!
library(scales)

options(warn=1) # http://stackoverflow.com/questions/11239428/how-to-change-warn-setting-in-r

source("disk_funcs.R")

# tidyr 0.4.0 Nested data frames
# http://blog.rstudio.org/2016/02/02/tidyr-0-4-0/

# Ãåíåðèì òî÷êè âíóòðè îêðóæíîñòè åäèíè÷íîãî ðàäèóñà.
# Ñ÷èòàåì, ÷òî âñå ÷àñòèöû åäèíè÷íîãî çàðÿäà, ïîýòîìó åãî îïóñêàåì
# df.particles <- data.frame(x = c(0.1, 0.2, 0.3), y = c(0.3, 0.4, 0.5), fixed = c(FALSE, FALSE, FALSE))
df.particles <- data.frame(j = (1:4)) %>%
  mutate(angle = runif(n(), min=0, max=2*pi), r = runif(n(), min=0, max=1)) %>%
  mutate(x = r*cos(angle), y = r*sin(angle), fixed = FALSE) %>%
  select(-angle, -r, -j)

# lapply(particles, function(x){print(paste0("-", x, "-"))})

# äëÿ ñõîäèìîñòè çàäà÷è ãåíåðèðóåì òàêæå çàôèêñèðîâàííûå òî÷êè íà îêðóæíîñòè
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
  # ïðîâåðÿåì, ÷òî òî÷êè íå óøëè èç îêðóæíîñòè åäèíè÷íîãî ðàäèóñà
  if(nrow(filter(dfs, !fixed, x^2+y^2 > 1.2)) > 0) break
  
  # ïðîâåäåì ðàñ÷åò ñèë è ïðèíóäèòåëüíî çàíóëèì âñå ñèëû, äåéñòâóþùèå íà òî÷êè íà îêðóæíîñòè
  dfs <- calc_qstep(dfs) %>%
    mutate(force.x = force.x * !fixed, force.y = force.y * !fixed)
  
  # gp <- disk_plot(dfs)
  # print(gp)
  
  # îïðåäåëèì ìàêñèìàëüíóþ ñèëó, äåéñòâóþùóþ íà ÷àñòèöó
  max.force <- max(sqrt(dfs$force.x ^ 2 + dfs$force.y ^ 2))
  
  # ïðîâîäèì ñìåùåíèå òî÷åê
  dfs %<>%
    mutate(x.old = x, y.old = y) %>%
    mutate(x = x + force.x / 1e4, y = y + force.y / 1e4)
  
  step <- step + 1
  print(paste0(
    "iteration #",
    step,
    " Ðàñ÷åò äëèòñÿ ",
    round(as.numeric(difftime(Sys.time(), start.time, unit = "sec")), digits = 0),
    # round(Sys.time() - start.time, digits = 0),
    " ñåê, max force = ",
    max.force
  ))
}

filter(dfs, !fixed, x^2+y^2 > 1)

gp <- disk_plot(dfs)
print(gp)



print("End...")
stop()

# =========================== ìàïèðóåì êîîðäèíàòû íà  GIS ==========================================
# ïîïðîáóåì îïòèìèçèðîâàòü ìàðøðóò îáõîäà
# 1. Óáèðàåì ôèêñèðîâàííûå òî÷êè
df.calc <- filter(dfs, !fixed)
# 2. Âûáèðàåì â êà÷åñòâå íà÷àëüíîé òî÷êè äàò÷èê, ìàêñèìàëüíî áëèçêî ðàñïîëîæåííûé ê êðàþ ïîëÿ
n1 <- filter(df.calc, x == max(x))

max(df.calc$x)
stop()


# =========================== ìàïèðóåì êîîðäèíàòû íà  GIS ==========================================
# ñâÿçü ýëëèïñà ñ îêðóæíîñòüþ
# http://stu.sernam.ru/book_ehm.php?id=38
# http://www.mathprofi.ru/linii_vtorogo_poryadka_ellips_i_okruzhnost.html
# http://math.stackexchange.com/questions/597090/computing-a-matrix-to-convert-an-x-y-point-on-an-ellipse-to-a-circle

# write.csv(dfs, file="sensors_calc.csv")
# https://yandex.ru/maps/?text=54.860510,38.655286&z=5
# Ïîëóðÿäèíêè
x0.geo <- 38.655286 # Longitude
y0.geo <- 54.860510 # Latitude
rx.geo <- sqrt((x0.geo - 38.662979)^2 + (y0.geo - 54.860416)^2) # ïðàâàÿ ãîðèçîíòàëüíàÿ òî÷êà
ry.geo <- sqrt((x0.geo - 38.655267)^2 + (y0.geo - 54.865068)^2) # âåðõíÿÿ âåðòèêàëüíàÿ òî÷êà

# Çîîëîãè÷åñêàÿ 2
x0.geo <- 37.58022 # Longitude
y0.geo <- 55.76559 # Latitude
rx.geo <- sqrt((0.01)^2) # ïðàâàÿ ãîðèçîíòàëüíàÿ òî÷êà
ry.geo <- sqrt((0.01)^2) # âåðõíÿÿ âåðòèêàëüíàÿ òî÷êà


# à åùå íå çàáûâàåì, ÷òî íàäî ïðåîáðàçîâûâàòü êîîðäèíàòû èç ìåòðîâ â ãðàäóñû
# Âû÷èñëåíèå ðàññòîÿíèÿ è íà÷àëüíîãî àçèìóòà ìåæäó äâóìÿ òî÷êàìè íà ñôåðå
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


# ñ÷èòàåì äëÿ ÷àñòèöû ñèëó âîçäåéñòâèÿ îñòàëüíûõ ÷àñòèö, ïðåäîñòàâëåííûõ ñïèñêîì
calc_force <- function(particle, plist){
  # plist -- ñïèñîê ÷àñòèö
  # print(paste0("(", particle[[1]], ", ", particle[[2]], ") <-> ", plist))
  
  # èñïîëüçóåì âåêòîðèçàöèþ ôóíêöèé è ñâîéñòâî äîïîëíåíèÿ
  #dd <- lapply(plist, function(x){})
  #dd <- plist - particle
  #print(dd)
} 

# http://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters
t <- lapply(particles, calc_force, plist = particles)
