hgroup.enum <- function(date, hour_bin=NULL, min_bin=5){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется hour.bin
  # отсчет измерений идет с 0:00
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  # если hour.bin=NULL, то идет привязка к интервалам min.bin, заданном в минутах
  
  tick_time <- date
  
  if (is.null(hour_bin)){
    # привязываем к минутным интервалам
    n <- floor(minute(tick_time)/min_bin)
    dt <- floor_date(tick_time, unit="hour") + minutes(n * min_bin)
    
  }else{
    # привязываем к часовым интервалам
    if (hour_bin < 1 & !(hour_bin %in% c(0.25, 0.5))) hour_bin=1
    n <- floor((hour(tick_time)*60 + minute(tick_time))/ (hour_bin*60))
    dt <- floor_date(tick_time, unit="day") + minutes(n * hour_bin*60)
  }
  
  dt
}

processScroll <- function(n, scroll_id, req_size){
  cat("iteration", n, "\n")
  
  scroll_json <- scroll(scroll_id=scroll_id, raw=TRUE, size=req_size)
  # jsonlite::prettify(scroll_res)
  # browser()
  # http://stackoverflow.com/questions/35198991/tidyjson-is-there-an-exit-object-equivalent
  # делаем в два шага
  
  # ускоряем сборку ~ в 2 раза путем выноса общего процессинга за скобки
  df0 <- scroll_json %>% 
    enter_object("hits") %>% 
    enter_object("hits") %>%
    gather_array %>% 
    enter_object("_source")
  
  df <- df0 %>%
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
              df0 %>%
                enter_object("dest") %>%
                spread_values(
                  dst_ip=jstring("ip"),
                  dst_port=jnumber("port")
                ) %>% select(-document.id),
              by = c('array.index'))
  
  df
}

