library(ggplot2)
# Example on rejection sampling
set.seed(10052019)

func <- function(x) {
  x^3 + x^2
}

m = 10^4
x = runif(m, -1,1)
y = runif(m, 0, 2)
s = x[y < func(x)]

ggplot(data.frame(x,y), aes(x, y)) +
  geom_point(data = data.frame(x, y = func(x)), aes(x,y), size = 0.3) +
  geom_point(size = 0.5, alpha = 0.5, color = "red") + 
  geom_point(data = data.frame(x = s,y = y[y< func(x)]), 
             aes(x, y), size = 0.5, alpha = 0.5, color = "green") 






