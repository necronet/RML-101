library(bayess)
library(mnormt)
data(normaldata)

shift=normaldata[,2]
hist(shift,nclass=10,col="steelblue",prob=TRUE,main="")


qqnorm((shift-mean(shift))/sd(shift),pch=19,col="gold2") 
abline(a=0,b=1,lty=2,col="indianred",lwd=2)

n = length(shift)
mmu = sum(shift)/(n+1)

vmu=0.75^2/(n+1)


stmu=(2+(n-1)*var(shift))/((n+2)*(n+1))


curve(dmt(x,mean=mmu,S=stmu,df=n+2),col="chocolate2",lwd=2, xlab="x",ylab="",xlim=c(-.5,.5))
curve(dnorm(x,mean=mmu,sd=sqrt(vmu)),col="steelblue2", lwd=2,add=TRUE,lty=2)