hgroup.enum <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения. 
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  
  tick_time <- date
  if (time.bin < 1 & !(time.bin %in% c(0.25, 0.5))) time.bin = 1
  n <- floor((hour(tick_time)*60 + minute(tick_time))/ (time.bin * 60))
  floor_date(tick_time, unit = "day") + minutes(n * time.bin *60)
}


date_format_tz <- function(format = "%Y-%m-%d", tz = "UTC") {
  function(x)
    format(x, format, tz = tz)
}