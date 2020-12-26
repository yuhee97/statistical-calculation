# 1
library(boot)

n <- 25 # 25, 50, 100, 200, 400
m <- 2000
alpha <- 0.05
LCL<-numeric(m)
UCL<-numeric(m)
lclb<-numeric(m)
uclb <-numeric(m)
lclp <-numeric(m)
uclp<-numeric(m)

for (i in 1:m){
  data <- rchisq(n, df= 2)
  LCL[i] <- (n-1)*var(data)/qchisq(1-alpha/2, df = n-1)
  UCL[i] <- (n-1)*var(data)/qchisq(alpha/2, df = n-1)
  
  cl_fun <- function(x, j){
    var(x[j])}
  
  boot.obj <- boot(data, R = 1000, statistic=cl_fun)
  res <- boot.ci(boot.obj, type=c("basic","perc"))
  
  lclb[i] <- res$basic[4]
  uclb[i] <- res$basic[5]
  lclp[i] <- res$percent[4]
  uclp[i] <- res$percent[5]
}
mean(LCL<4 & UCL>4)
mean(lclb<4 & uclb>4)
mean(lclp<4 & uclp>4)

# 2
n_size <- c(25, 50, 100, 200, 400)
R <- 10000
power_t <- numeric(R)
power_ks <- numeric(R)
p.reject_t <- numeric(length(n_size))
p.reject_ks <- numeric(length(n_size))
j=1

for(n in n_size){
  
  for (i in 1:R){
    p<-rbinom(n,1,0.5)
    x<-p*rnorm(n,-0.5,0.2)+(1-p)*rnorm(n,0.5,0.2)
    y<-rnorm(n,0,sqrt(0.29))
    
    pvalue_t <- t.test(x,y)$p.value
    power_t[i] <- as.integer(pvalue_t<0.05)
    
    pvalue_ks <- ks.test(x,y)$p.value
    power_ks[i] <- as.integer(pvalue_ks<0.05)
  }
  p.reject_t[j] <- mean(power_t)
  p.reject_ks[j] <- mean(power_ks)
  j=j+1
}
p.reject_t
p.reject_ks

# 3
options(warn=-1)
n <- 25 # 25 50 100 200
R <- 1000
B <-1000
power_ks <- numeric(B)
power_b <- numeric(B)
test_bootstrap <- numeric(R)

for(j in 1:R){
  
  p<-rbinom(n,1,0.5)
  x<-p*rnorm(n,-0.5,0.2)+(1-p)*rnorm(n,0.5,0.2)
  y<-rnorm(n,0,sqrt(0.29))
  v <- ks.test(x,y)$statistic
  n1 <- length(x)
  n2 <- length(y)
  z <- c(x,y)
  counter <- 0
  
  pvalue_ks <- ks.test(x,y)$p.value
  power_ks[j] <- as.integer(pvalue_ks<0.05)
  
  for (i in 1:B){
    xstar <- sample(z, n1, replace = T)
    ystar <- sample(z, n2, replace = T)
    vstar <- ks.test(xstar,ystar)$statistic
    if(vstar >= v){counter <- counter+1}
    
  }
  pvalue <- counter/B
  test_bootstrap[j]<-as.integer(pvalue<0.05)
}
mean(test_bootstrap)
mean(power_ks)

# 4
# type 1 error estimation under the null hypothesis
n<-25 # 25, 50, 100, 200
B<- 1000 
p<-0.2
alpha<-0.05
num_iter<-1000
test_bootstrap<-numeric(num_iter)

for (i in 1:num_iter){
  sigma <- sample(c(1, 10), size = n,replace = TRUE, prob = c(1-p, p))
  data<-rnorm(n, 0, sigma) 
  data_ext<-c(data, 2*mean(data)-data)
  r <- data-mean(data)
  l <- mean(data)-data
  v <-ks.test(r,l)$statistic
  counter <- 0
  
  for (b in 1:B) {
    bdata<-sample(data_ext,size=n,replace=TRUE) 
    xr <- bdata-mean(bdata) 
    xl <- mean(bdata)-bdata
    vstar <- ks.test(xr, xl)$statistic
    if (abs(vstar) >= abs(v)){counter<-counter+1}
  }
  pvalue <- counter/B
  test_bootstrap[i]<-as.integer(pvalue<alpha)
}
mean(test_bootstrap)
# power estimation under the alternative hypothesis
n<-25 # 25, 50, 100, 200
B<- 1000 
p<-0.2
alpha<-0.05
num_iter<-1000
test_bootstrap<-numeric(num_iter)

for (i in 1:num_iter){
  data <- rexp(n)
  data_ext<-c(data, 2*mean(data)-data)
  r <- data-mean(data)
  l <- mean(data)-data
  v <-ks.test(r,l)$statistic
  counter <- 0
  
  for (b in 1:B) {
    bdata<-sample(data_ext,size=n,replace=TRUE) 
    xr <- bdata-mean(bdata) 
    xl <- mean(bdata)-bdata
    vstar <- ks.test(xr, xl)$statistic
    if (abs(vstar) >= abs(v)){counter<-counter+1}
  }
  pvalue <- counter/B
  test_bootstrap[i]<-as.integer(pvalue<alpha)
}
mean(test_bootstrap)

# 5
t <- c(94, 197, 16, 38, 99, 141, 23)
c <- c(52, 104, 146, 10, 51, 30, 40, 27, 46)
z <- c(t,c)
B <- 1000
testtotal <- numeric(B)

for (i in 1:B){
  experiment <- sample(z)
  newt <- experiment[1:length(t)]
  newc <- experiment[(length(t)+1):length(z)]
  testtotal[i] <- t.test(newt, newc, alternative="greater")$statistic
}

original <- (86.86-56.22)/sqrt(25.24^2+14.14^2)
mean(testtotal >= original)

# 6
library(DAAG); attach(ironslag)
k <- 100
e1 <- e2 <- e3 <- e4 <-c()

for (n in 1:k){
  testIndexes <- sample(1:nrow(ironslag), 10)
  
  testData <- ironslag[testIndexes, ]
  trainData <- ironslag[-testIndexes, ]
  
  J1 <- lm(magnetic ~ chemical, data=trainData)
  yhat1 <- predict(J1,testData)
  e1 <- c(e1,testData$magnetic - yhat1)
  
  J2 <- lm(magnetic ~ chemical + I(chemical^2),data=trainData)
  yhat2 <- predict(J2,testData)
  e2 <- c(e2,testData$magnetic - yhat2)
  
  J3 <- lm(log(magnetic) ~ chemical,data=trainData)
  logyhat3 <- predict(J3,testData)
  yhat3 <- exp(logyhat3)
  e3 <- c(e3,testData$magnetic - yhat3)
  
  J4 <- lm(log(magnetic) ~ log(chemical),data=trainData)
  logyhat4 <- predict(J4,testData)
  yhat4 <- exp(logyhat4)
  e4 <- c(e4,testData$magnetic - yhat4)
}

c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2))
