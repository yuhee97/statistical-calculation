# 1
library(stats4)

B<-10000
n <- 100

lcl <- numeric(B)
ucl <- numeric(B)

theta_total <- replicate(B, expr={
  x <- rcauchy(n, location=2, scale=1)
  mlogL <- function(theta){
    nlk <- length(x)*log(pi)+sum(log(1+(x-theta)^2))
    return(nlk)
  }
  fit<-mle(mlogL,start=list(theta=median(x)))
  c<-qnorm(1-0.05/2)
  lcl <- fit@coef-c*sqrt(diag(fit@vcov))
  ucl <- fit@coef+c*sqrt(diag(fit@vcov))
  c(lcl, ucl)
})
mean(theta_total[1,]<2 & theta_total[2,]>2)

# 2
library(stats4)

n <- 50
m <- 10000

comp.mse <- function(n, m, p) {
  smean <- numeric(m)
  med<-numeric(m)
  mle<-numeric(m)
  for (i in 1:m) {
    x <- rcauchy(n, location=2, scale=1)
    smean[i] <-mean(x)
    med[i]<-median(x)
    mlogL <- function(theta){
      nlk <- length(x)*log(pi)+sum(log(1+(x-theta)^2))
      return(nlk)
    }
    fit<-mle(mlogL,start=list(theta=1))
    mle[i] <- fit@coef
  }
  mse_est_smean <- mean((smean-2)^2)
  mse_est_med <- mean((med-2)^2)
  mse_est_mle <- mean((mle-2)^2)
  
  return(c(mse_est_smean,mse_est_med,mse_est_mle))
}
comp.mse(n,m,p)

# 3
mlogL<-function(theta){
  nlk<-(-1)*(1997*(log(2+theta)-log(4))+1810*(log(1-theta)-log(4))+32*(log(theta)-log(4)))
  return(nlk)
}

fit <- optim(par=1/2, mlogL, upper=1.01, lower=-0.01, method = "Brent", hessian = T)

fit$par

c<-qnorm(1-0.05/2)
cbind(fit$par-c*sqrt(diag(solve(fit$hessian))),
      fit$par+c*sqrt(diag(solve(fit$hessian))))

# 4
library(numDeriv)
mlogL<-function(gamma){
  nlk <- 77*(gamma+log(1-exp(-(gamma))))-145*log(gamma)+25*log(2)+12*log(6)+5*log(24)+log(120)
  return(nlk)
}

mlogL_deriv<-function(gamma){
  grad(mlogL,gamma)
}

newton <- function(fun, tol = 1e-7, x0, N = 300){
  i <- 1
  x1 <- x0
  p <- numeric(N)
  while(i <= N){
    df.dx <- grad(fun,x0)
    x1 <- (x0 - (fun(x0) / df.dx))
    p[i] <- x1
    i = i+1
    if(abs(x1 - x0) < tol) break
    x0 = x1
  }
  return(p[1:(i - 1)])
}

data<-rep(1:6,c(34,25,12,5,1,0))
res<-newton(mlogL_deriv,x0=mean(data))
theta_mle<-res[length(res)]
theta_mle
se_est<-as.numeric(sqrt(1/hessian(mlogL, theta_mle)))
se_est

# 5
library(numDeriv)
mlogL<-function(theta){
  p<-theta[1]
  d<-theta[2]
  nlk<-(-1)*(32*log((1-d)*p+(d*(p^2)))+41*log(2*d*p*(1-p))+36*log((1-d)*(1-p)+d*((1-p)^2)))
  return(nlk)
}
mlogL_deriv<-function(theta){
  grad(mlogL,theta)
}
newton_mul <- function(fun, tol = 1e-7, x0, N = 300){
  i <- 1
  x1 <- x0
  p <- matrix(0,length(x0),N)
  while(i <= N){
    df.dx <- jacobian(fun,x0)
    x1 <- x0 - solve(df.dx) %*% fun(x0) 
    p[,i] <- x1
    i = i+1
    if(sum(abs(x1 - x0)) < tol) break
    x0 = x1
  }
  return(p[,1:(i - 1)])
}
res_mul<-newton_mul(mlogL_deriv,x0=c(0.5,0.5))
theta_mle<-res_mul[,dim(res_mul)[2]]
theta_mle

c<-qnorm(1-0.05/2)
cbind(theta_mle[1]-c*sqrt(diag(solve(hessian(mlogL,x=theta_mle))))[1],
      theta_mle[1]+c*sqrt(diag(solve(hessian(mlogL,x=theta_mle))))[1])
cbind(theta_mle[2]-c*sqrt(diag(solve(hessian(mlogL,x=theta_mle))))[2],
      theta_mle[2]+c*sqrt(diag(solve(hessian(mlogL,x=theta_mle))))[2])

# 6
set.seed(123)
n <-1000000
x1 <- rnorm(n)           
x2 <- rnorm(n)
x3 <- rnorm(n)
x <- cbind(x1,x2,x3)
z <- 1 + 0.5*x1 -0.4*x2+0.7*x3       
pr <- 1/(1+exp(-z))        
y <- rbinom(n,1,pr)      
df <- data.frame(y=y,x1=x1,x2=x2,x3=x3)

library(numDeriv)
alpha <- 0.5
num_iters <- 1000
theta_history <- list(num_iters)

folds <- cut(seq(1,nrow(df)),breaks=1000,labels=FALSE)
beta <- matrix(c(0,0,0,0), nrow=4)

for (i in 1:num_iters) {
  testIndexes <- which(folds==i,arr.ind=TRUE)
  dfm <- df[testIndexes, ]
  mlogLp <- function(beta){
    x <- cbind(dfm$x1, dfm$x2, dfm$x3)
    lam <- 1/(1+exp(-cbind(1,x) %*% beta))
    nlk <- (-1)*(mean(log(lam)*dfm$y+log((1-lam))*(1-dfm$y)))
    return(nlk)
  }
  delta <- grad(mlogLp , beta)
  beta <- beta - alpha * delta
  theta_history[[i]] <- beta
}
print(beta)

glm(y~x1+x2+x3,data=df,family="binomial")
