#Inverse CDF sampling
library(ggplot2)

CDF <- function(x, lambda, k) {
  1 - exp ( - (x/lambda) ) ^k  
}

U1 <- runif(100000, 0, 1)
k1 <- 11
lambda <- 1
# inverse CDF
x1star <- lambda * ( - log ( 1 - U1) ) ^ (1/k1)

ggplot(data.frame(x=U1, y = CDF(U1, lambda, k1)), aes(x,y)) + geom_line() 
ggplot(data.frame(x=U1, y = x1star), aes(y)) + geom_histogram(bins = 70)