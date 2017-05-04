hgroup.enum <- function(date, time.bin=4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  
  tick_time <- date
  if (time.bin < 1 & !(time.bin %in% c(0.25, 0.5))) time.bin=1
  n <- floor((hour(tick_time)*60 + minute(tick_time))/ (time.bin * 60))
  floor_date(tick_time, unit="day") + minutes(n * time.bin *60)
}

processScroll <- function(n, scroll_id, req_size){
  cat("iteration", n, "\n")
  
  scroll_json <- scroll(scroll_id=scroll_id, raw=TRUE, size=req_size)
  
  # http://stackoverflow.com/questions/35198991/tidyjson-is-there-an-exit-object-equivalent
  # делаем в два шага
  df <-
    scroll_json %>% enter_object("hits") %>% enter_object("hits") %>%
    gather_array %>% enter_object("_source") %>%
    spread_values(
      start_time=jstring("start_time")
    ) %>%
    enter_object("source") %>%
    spread_values(
      src_ip=jstring("ip"),
      src_port=jnumber("port")
    ) %>%
    enter_object("stats") %>%
    spread_values(
      bytes_total=jnumber("net_bytes_total")
    ) %>%
    select(-document.id)
  
  # then enter an object and get something from inside, merging it as a new column
  df <- merge(df, 
              scroll_json %>% enter_object("hits") %>% enter_object("hits") %>%
                gather_array %>% 
                enter_object("_source") %>%
                enter_object("dest") %>%
                spread_values(
                  dst_ip=jstring("ip"),
                  dst_port=jnumber("port")
                ) %>% select(-document.id),
              by = c('array.index'))
  
  df
}

