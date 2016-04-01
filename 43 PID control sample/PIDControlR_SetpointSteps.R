# Step change setpoint profile for PID control simulation
SP = rep(10, length(tt))
SP[which(tt >= 30)] = 5
SP[which(tt >= 60)] = 20