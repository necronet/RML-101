library(ggplot2)
library(FNN)

# Require functions
#Epanechnikov quadratic kernel

K_quadratic_kernel <- function(x, x0, lambda = 0.2) {
  D ((abs(x - x0))/ lambda)
}

K_quadratic_kernel(1,10)

D <- function(t) {
  if (abs(t) <= 1) {
    (3/4) * (1 - t^2)
  } else {
    0
  }
}

set.seed(11052019)
scalar <- 4
upper_x_limit <- 1
k <- 30
n <- 100
e <- rnorm(n, sd=1/3)
x <- seq(0, upper_x_limit, length=n)
y <- sin(scalar*x) + e

data <- data.frame(x,y)

y_estimator <- knn.reg(x, y = y, k = k)

# Kernel Smoother with Nadaraya-Watson weighted aerage
y_smoothed <- vector()
for (x0 in x) {
  num <- sum(apply(as.data.frame(x), 1, function(xi){K_quadratic_kernel(x0, xi)})*y)
  den <- sum(apply(as.data.frame(x), 1, function(xi){K_quadratic_kernel(x0, xi)}))
  y_smoothed<-append(y_smoothed,num/den)
}


## Ploting the results
data<-cbind(data, y_smoothed)
ggplot(data) + geom_point(aes(x=x, y=y)) + 
  geom_line(aes(x=x, y=y_estimator$pred), color='green') + 
  geom_line(aes(x=x, y=y_smoothed), color='red')
