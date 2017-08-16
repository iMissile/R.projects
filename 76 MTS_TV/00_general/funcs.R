hgroup.enum <- function(date, hour_bin=NULL, min_bin=5){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется hour.bin
  # отсчет измерений идет с 0:00
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  # если hour.bin=NULL, то идет привязка к интервалам min.bin, заданном в минутах
  # необходим пакет lubridate
  
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

# взято из formattable::formatter.R
dvt_color_bar <- function(color = "lightgray", fun = "proportion", ...) {
  fun <- match.fun(fun)
  formatter("span",
            style = function(x) style(
              display = "inline-block",
              direction = "ltr", # "inherit", # "rtl",
              "border-radius" = "4px",
              "padding-right" = "2px",
              "background-color" = csscolor(color),
              width = percent(fun(as.numeric(x), ...))
            ))
}

buildReqFilter <- function(field, conditions, add=TRUE){
  ifelse(is.null(conditions) || conditions=="all", " ",
         stri_join(ifelse(add, " AND ", " "), field, " IN (",
                   stri_join(conditions %>% map_chr(~stri_join("'", .x, "'", sep="")),
                             sep = " ", collapse=","),
                   ") ", sep = "", collapse=""))
}

# конструирование ограничений запроса по данным фильтров
buildReqLimits <- function(begin, end, region=NULL, prefix=NULL, channel=NULL) {
  # region, prefix, channel -- вектора
  # базисная SQL конструкция для ограничения дат ----
  # limit_dates <- paste0(" toDate(begin) >= toDate('", begin, "') AND toDate(end) <= toDate('", end, "') ")
  limit_dates <- paste0(" date >= '", begin, "' AND date <= '", end, "' ")
  
  # добавочная SQL конструкция для ограничения регионов -----
  limit_regions <- 

  paste0(limit_dates, 
         " AND ", buildReqFilter("region", region), 
         " AND ", buildReqFilter("prefix", prefix),
         " AND ", buildReqFilter("channelId", channel)
         )
}

