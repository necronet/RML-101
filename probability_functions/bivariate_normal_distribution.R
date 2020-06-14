library("MASS") 

N <- 1000
mu <- c(5, 2)
S <- matrix(c(1, .5, 1, .5), ncol = 2)

X <- mvrnorm(n = N, mu = mu, Sigma = S)

plot(X)

plot(density(X[,1]))
plot(density(X[,2]))


par(mfrow=c(2,2))
plot(X, main="Scatter plot")

plot(density(X[,1]),main="Density X1")
plot(density(X[,2]),main="Density X2")
hist(X[,1], main="Histogram of X1")
dev.off()




