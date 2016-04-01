# Implementation of a PID control algorithm in R. A simple process for testing the controller. 

pv = function(pv.prev, u, tt) {
  out = pv.prev*1.1 + .5           # exponential growth + linear growth 
  out = out - 0.1*u                # the control response
  out = out + .5*runif(length(tt)) # a little noise, just for fun
  
  if (out < 0) out = 0             # keep values positive
  return(out)
}