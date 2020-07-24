seq(0, 1, by = .1)

print(7/13 - 3/31, digit = 22)

# Subscripting inferno 
amat = matrix(1:100, 10, 10)
bmat <- matrix(0L, nrow(amat)/2, ncol(amat))

for(i in 1:nrow(bmat)) bmat[i,] <- amat[2*i-1,] * amat[2*i,]


# Better with subscripting

bmat <- amat[seq(1, nrow(amat), by=2),] * amat[seq(2, nrow(amat), by=2),]


x <- 0.5
if(x < 1) y <- -1 else y <- 1
print(y)


# A warning will be trigger
x <- c(10, 0.5, 10)
if(x < 1) y <- -1 else y <- 1
print(y)


# Fix by using ifelse
y <- ifelse( x < 1, -1, 1)
print(y)
# Another option
sign(x - 1)



