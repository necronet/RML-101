# Metropolis Algorithm for MCMC
# Based on https://www.youtube.com/watch?v=h1NOS_wxgGg
n  =250000
x = rep(0, n)
x[1] = 0.5

for (i in 1:n) {
  x_c = rnorm(1, x[i], 0.05)
  rand = runif(1,0,1)
  if (rand < min(1, dnorm(1, x_c)/dnorm(1, x[i]) )) {
    x[i+1] = x_c
  } else {
    x[i+1] = x[i]
  }
}

ggplot(data.frame(x), aes(x)) + geom_histogram(bins = 70)