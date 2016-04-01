# Initialization of the PID control simulation 

# controller parameters
Kp = 10                 # proportional gain
Ti = 1                  # integral time
Td = 0.01               # derivative time

# simulation parameters
dt = .1                 # time step
tt = seq(0, 100, by=dt) # time vector

# initialize the following to a vector of zeros
# as long as the time variable tt
# - PV, process variable
# - U, control output
# - E, error
# - EI, error integral
# - ED, error derivative
PV = U = E = EI = ED = rep(0, length(tt))
PV[1] = 5 # initial state of the process variable