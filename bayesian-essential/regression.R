library(bayess)
data(caterpillar)


X <- caterpillar[1:8]
y <- log(caterpillar[,9])
vnames = paste0("X",1:8)

par(mfrow=c(2,4),mar=c(4.2,2,2,1.2))
for (j in 1:8) 
  plot(X[,j],y,xlab=vnames[j],pch=19, col="sienna4",xaxt="n",yaxt="n")


X <- scale(X)


