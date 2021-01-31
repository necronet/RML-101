amps <- c(.6, .25, .1, .05)
N <- 4
time_unit <- .001
ts <- seq(from = 0, to = N - 1) / N  * time_unit

max_freq <- N / time_unit / 2
fs = seq(from = 0, to = N - 1) / N * max_freq
args = ts %*% t(fs)
M = cos(args * 2 * pi)

# Not an identity just yet
M %*% t(M)


ts = (0.5 + seq(from = 0, to = N - 1)) / N
fs = (0.5 + seq(from = 0, to = N - 1)) / 2
args = ts %*% t(fs)
M = cos(2 * pi * args)

ys = cos(ts %*% t(fs)) %*% amps

# 2I matrix 
M %*% t(M)

M = cos(2 * pi * ts %*% t(fs))
#Getting amps back
amps = M * ys / 2

