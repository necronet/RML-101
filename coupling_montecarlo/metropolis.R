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

#Metropolis-Hasting based on 

target <- function(x){
  if(x<0){
    return(0)
  }
  else {
    return( exp(-x))
  }
}


library(animation)

animation::saveGIF( {
  
#oopt <- ani.options(interval = 0.2, nmax = 10)

x = rep(0,50000)
x[1] = 3
for(i in 2:50000){
  currentx = x[i-1]
  proposedx = currentx + rnorm(1,mean=0,sd=1)
  A = target(proposedx)/target(currentx) 
  if(runif(1)<A){
    x[i] = proposedx       # accept move with probabily min(1,A)
  } else {
    x[i] = currentx        # otherwise "reject" move, and stay where we are
  }
  
  # if(i %% 1000 == 0) {
  #   dev.hold()
  #   hist(x, main = paste0("MCMC Metropolis hasting algorithm, iteration: ",i))
  #   ani.pause()
  # }
}
}, img.name = "MCMC-metropolis-hasting", title = "MCMC Metropolis hasting algorithm", 
description = "To approximate posterior with target as exponential distribution")


hist(x)
ggplot(data.frame(x = 1:length(x), y = x), aes(x, y)) + geom_line()



