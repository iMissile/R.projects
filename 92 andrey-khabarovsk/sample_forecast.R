function(timeseries, h_forecast, frequency_ts)
{
  forecast_model <- es(timeseries, "ZZZ", h=h_forecast, frequency = frequency_ts);
  rezult <- (forecast_model$forecast);
  return(rezult) 
}

ts_buffer <- na.omit(total_for_forecast$Qty)
ts_model <- ts(ts_buffer, frequency = 7, start = 1)