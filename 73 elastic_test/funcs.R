hgroup.enum <- function(date, hour_bin=NULL, min_bin=5){
  # ����������� ��� ���������, ������� ������ � ���������� [0, t] � ����� ���������.
  # ����� ��������� ����� ���� ������ 1, 2, 3, 4, 6, 12 �����, ������������ hour.bin
  # ������ ��������� ���� � 0:00
  # �������� ��� �����������. ��� ����������� ������ ���� ����������� ��������� ����� ������ 1
  # 0.5 -- ��� � �������.0.25 -- ��� � 15 �����
  # ���� hour.bin=NULL, �� ���� �������� � ���������� min.bin, �������� � �������
  
  tick_time <- date
  
  if (is.null(hour_bin)){
    # ����������� � �������� ����������
    n <- floor(minute(tick_time)/min_bin)
    dt <- floor_date(tick_time, unit="hour") + minutes(n * min_bin)
    
  }else{
    # ����������� � ������� ����������
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
  # ������ � ��� ����
  
  # �������� ������ ~ � 2 ���� ����� ������ ������ ����������� �� ������
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

