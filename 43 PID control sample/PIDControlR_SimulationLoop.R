# Simulation loop for a PID controller in R f

or (k in 2:length(tt)) {
  PV[k] = pv(PV[k-1], U[k-1], tt[k])
  E[k] = PV[k] - SP[k]
  
  EI[k] = EI[k-1] + E[k]*dt  # integral
  ED[k] = (E[k] - E[k-1])/dt # derivative
  
  U[k] = Kp*(E[k] + (1/Ti)*sum(E*dt) + Td*ED[k]) 
  
  if (U[k] < 0) U[k] = 0
}