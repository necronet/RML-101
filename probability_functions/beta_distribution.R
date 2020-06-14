# plots chapter 1

B <-  function(alpha, beta) {
  (gamma(alpha)*gamma(beta))/gamma(alpha+beta)
}

BetaDist <- function(x, alpha = 0.5, beta = 0.5) {
     ( ( x^(alpha-1) ) * (1 - x) ^ (beta - 1) ) / B(alpha, beta)
}

x <- seq(0,1, length = 100)
y1 <- BetaDist(x, 10, 10)
y2 <- BetaDist(x, 2, 2)
y3 <- BetaDist(x, 1, 1)
y4 <- BetaDist(x, 40, 40)
y5 <- BetaDist(x, 1, 3)
y6 <- BetaDist(x, 1, 10)
y7 <- BetaDist(x, 3, 1)
y8 <- BetaDist(x, 10, 1)

plot(x, y4, type="l")
lines(x, y2, type="l", col=2)
lines(x, y3, type="l", col=3)
lines(x, y1, type="l", col=4)
lines(x, y5, type="l", col=5)
lines(x, y6, type="l", col=6)
lines(x, y7, type="l", col=7)
lines(x, y8, type="l", col=8)

x <- runif(1000)
plot(x, BetaDist(x, 10, 10), type="l")
