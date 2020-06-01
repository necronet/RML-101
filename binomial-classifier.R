# Using a multinomial classification gmlnet
library(glmnet)

load("glmnet/QuickStartExample.RData")


fit = glmnet(x, y)


plot(fit)

plot(fit)