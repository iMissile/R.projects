dfs <- readRDS("sensors_calulated.rds")

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

min.length <- NA
for (cur.path in routes){
  cur.length <- calc_path_length(cur.path) 
  print(cur.length)
  
  if (is.na(min.length) || cur.length < min.length){
    best.route <- list(s = cur.length, path = cur.path)
    min.length <- cur.length}
}

# лучший маршрут -- best.route
# пересортируем сенсоры в исходной таблице в соотв. с этой последовательностью
# объединяем лучший путь и делаем arrange

path.frame <- data.frame(j = best.route$path, j.path = 1:length(best.route$path))

df.best <- df.calc %>%
  left_join(path.frame, by = "j") %>%
  arrange(j.path)
  

stop()

t <- lapply(routes, calc_path_length)
