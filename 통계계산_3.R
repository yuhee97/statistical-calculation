# 1
rep_num<-100000
exp_profit<-function(n,m){ 
  X<-rbinom(rep_num,n,0.95)
  return(mean(X*1+(-m)*(X-100)*(X>100)))
}
num_ob<-100:110
profit <- matrix(0, 11, 4)
for (i in 2:5){
  for (n in 100:110){
    profit[n-99,i-1]<-exp_profit(n,i)
  }}
par(mfrow=c(2,2))
plot(num_ob, profit[,1],type="o", 
     main="비용이 2만원 일 때 수익의 기댓값 그래프",
     xlab="# of people", ylab="expectation profit", ylim=c(80,100))
abline(h=95,lty=4,col="blue")
points(num_ob[which.max(profit[,1])], max(profit[,1]), cex=1.3, pch=15, col="blue")
plot(num_ob, profit[,2],type="o", 
     main="비용이 3만원 일 때 수익의 기댓값 그래프",
     xlab="# of people", ylab="expectation profit", ylim=c(80,100))
abline(h=95,lty=4,col="blue")
points(num_ob[which.max(profit[,2])], max(profit[,2]), cex=1.3, pch=15, col="blue")
plot(num_ob, profit[,3],type="o", 
     main="비용이 4만원 일 때 수익의 기댓값 그래프",
     xlab="# of people", ylab="expectation profit", ylim=c(80,100))
abline(h=95,lty=4,col="blue")
points(num_ob[which.max(profit[,3])], max(profit[,3]), cex=1.3, pch=15, col="blue")
plot(num_ob, profit[,4],type="o", 
     main="비용이 5만원 일 때 수익의 기댓값 그래프",
     xlab="# of people", ylab="expectation profit", ylim=c(80,100))
abline(h=95,lty=4,col="blue")
points(num_ob[which.max(profit[,4])], max(profit[,4]), cex=1.3, pch=15, col="blue")

# 2
set.seed(12345)
km <- 0
kc <- 0
n<-0
for(i in 1:10000){
  while(TRUE){
    n <- n+1
    minsu <- rbinom(1, 1, 0.1)
    if(minsu!=0) {km <- km + 1; break}
    chursu <- rbinom(1, 1, 0.2)
    if(chursu!=0) {kc <- kc + 1; break}
  }
}
pm <- km/10000
pc <- kc/10000
pm
pc

# 3
n <- 30
alpha <- .05
CL <- replicate(10000, expr = {
  x <- rlnorm(n=n, mean=0, sd=1)
  LCL <- mean(x)-qt(1-alpha/2,df=n-1)*sqrt(var(x))/sqrt(n)
  UCL <- mean(x)+qt(1-alpha/2,df=n-1)*sqrt(var(x))/sqrt(n)
  c(LCL, UCL)
} )
mean(CL[1,]<exp(0.5)&CL[2,]>exp(0.5)) 

# 4
n <- 30
m <- 100000
smean <- numeric(m)   
medi <- numeric(m)  
tmean <- numeric(m)  
for (i in 1:m) {
  sigma <- sample(c(1, 10), size = n, replace = TRUE, prob = c(0.9, 0.1))
  x <- sort(rnorm(n, 0, sigma))
  smean[i] <- mean(x)
  medi[i] <- median(x)
  tmean[i] <- sum(x[2:29])/(n-2)
}  
mse.samplemean <- mean(smean^2)
mse.median <- mean(medi^2)
mse.trimmedmean <- mean(tmean^2)
cbind(mse.samplemean, mse.median, mse.trimmedmean)

# 5 -> 수기

# 6
n <- c(10, 20, 30, 40, 50)
m <- 1000
mu0 <- 500
sigma <- 100
mu <- c(seq(450, 650, 10)) 
M <- length(mu)
power <- numeric(M)
p.reject <-matrix(0, 21, 5)
for (j in 1:length(n)) {
  for (i in 1:M) {
    mu1 <- mu[i]
    pvalues <- replicate(m, expr = {
      x <- rnorm(n[j], mean = mu1, sd = sigma)
      ttest <- t.test(x,alternative = "greater", mu = mu0)
      ttest$p.value } )
    power[i] <- mean(pvalues <= .05)
  } 
  p.reject[,j] <- power
}
plot(mu,p.reject[,1], type="l", xlab="mean", ylab="power", col="#FF0000FF")
lines(mu, p.reject[,2], type="l", col="#00A600FF")
lines(mu, p.reject[,3], type="l", col="#FFC800FF")
lines(mu, p.reject[,4], type="l", col="#0008FFFF")
lines(mu, p.reject[,5], type="l", col="#FF00E6FF")
abline(h=0.05,lty=2)
abline(v=500, lty=2)
color <- c("#FF0000FF", "#00A600FF", "#FFC800FF", "#0008FFFF", "#FF00E6FF")
legend("right", legend=n, title=" 표본의 크기(n)", col=color, lty=1, cex=0.8, lwd=2)

# 7 -> 수기