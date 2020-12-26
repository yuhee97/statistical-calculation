# install.packages("sigmoid")
library(sigmoid)

## uniform
Unif <- function(y,n){
  pmat<-matrix(0,n+1,length(y))
  kvec<-seq(0,n,by=1)
  for (k in kvec){
    pmat[(k+1),]<-(-1)^k*choose(n,k)*(relu(n*y-k))^{n-1}*n/factorial(n-1)
  }
  f<-apply(pmat,2,sum)
  return(f)
}

y <- seq(0,1,length=200)

par(mfrow=c(2,2))
plot(y, Unif(y,2), type="l", main="n=2",ylim=c(0,6.5))
lines(y, dnorm(y, mean=1/2, sd=sqrt((1/12)/2)), col=4) 
abline(v=0.5,lty=2,col=2)

plot(y, Unif(y,5), type="l", main="n=5",ylim=c(0,6.5))
lines(y, dnorm(y, mean=1/2, sd=sqrt((1/12)/5)), col=4) 
abline(v=0.5,lty=2,col=2)

plot(y, Unif(y,10), type="l", main="n=10",ylim=c(0,6.5))
lines(y, dnorm(y, mean=1/2, sd=sqrt((1/12)/10)), col=4) 
abline(v=0.5,lty=2,col=2)

plot(y, Unif(y,20), type="l", main="n=20",ylim=c(0,6.5))
lines(y, dnorm(y, mean=1/2, sd=sqrt((1/12)/20)), col=4) 
abline(v=0.5,lty=2,col=2)

# exp
Exp <- function(y,n){ return(n/factorial(n-1)*((n*y)^(n-1))*exp(-n*y)) }

y <- seq(0,4,0.1)

par(mfrow=c(2,2))
plot(y, Exp(y,2), type="l", main="n=2",ylim=c(0,2.5))
lines(y, dnorm(y, mean=1, sd=sqrt(1/2)), col=4)
abline(v=1,lty=2,col=2)

plot(y, Exp(y,5), type="l", main="n=5",ylim=c(0,2.5))
lines(y, dnorm(y, mean=1, sd=sqrt(1/5)), col=4)
abline(v=1,lty=2,col=2)

plot(y, Exp(y,10), type="l", main="n=10",ylim=c(0,2.5))
lines(y, dnorm(y, mean=1, sd=sqrt(1/10)), col=4)
abline(v=1,lty=2,col=2)

plot(y, Exp(y,20), type="l", main="n=20",ylim=c(0,2.5))
lines(y, dnorm(y, mean=1, sd=sqrt(1/20)), col=4)
abline(v=1,lty=2,col=2)