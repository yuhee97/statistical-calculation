# 1
myfun1<-function(N)
{
  p <- 0
  for (n in 0:N) 
  {
    p <- 4*(-1)^n/(2 * n + 1) + p
  }
  return(p)
}
myfun2<-function(N)
{
  n <- c(0:N)
  sum(4*(-1)^n/(2 * n + 1))
}

library(microbenchmark)
microbenchmark(myfun1(10000),myfun2(10000),unit="ms")


# 2.1
rm(list = ls()) 
options(warn=-1)
library(tictoc)
library(parallel)
n_vec<-c(25,50,100,200)
iter_num<-200
B<-1000
alpha<-0.05
bootfun<-function(n){
  test_boot<-numeric(iter_num)
  for (j in 1:iter_num)
  {
    p<-rbinom(n,1,0.5)
    x<-p*rnorm(n,-0.5,0.2)+(1-p)*rnorm(n,0.5,0.2)
    y<-rnorm(n,0,sqrt(0.29))
    z<-c(x,y)
    v<-ks.test(x,y)$statistic
    vstar<-numeric(B)
    for(b in 1:B){
      xstar<-sample(z,n,replace = T)
      ystar<-sample(z,n,replace = T)
      vstar[b]<-ks.test(xstar,ystar)$statistic
    }
    test_boot[j]<-as.integer(mean(vstar>=v)<alpha)
  }
  return(mean(test_boot))
}
nworkers <- 4 
tic(paste("bootstrapping using parLapply and", nworkers, "workers"))
cl <- makeCluster(nworkers)
clusterExport(cl, c("n_vec", "iter_num", "B", "alpha")) 
result <- parLapply(cl, n_vec, bootfun)
result
with_time <- toc()
stopCluster(cl)

# 2.2
rm(list = ls()) 
options(warn=-1)
library(tictoc)
library(parallel)

tic(paste("bootstrapping using parLapply"))
n_vec<-c(25,50,100,200)
iter_num<-200
B<-1000
alpha<-0.05
resul<-numeric(length(n_vec))

for (i in seq_len(length(n_vec))){
  n<-n_vec[i]
  cat("Sample size ",n,"\n")
  
  nworkers <- 4 
  cl <- makeCluster(nworkers)
  clusterExport(cl, c("i", "n_vec", "iter_num", "B", "alpha")) 
  result <- parLapply(cl, 1:iter_num, function(x){
    n<-n_vec[i]
    p<-rbinom(n,1,0.5)
    xx<-p*rnorm(n,-0.5,0.2)+(1-p)*rnorm(n,0.5,0.2)
    y<-rnorm(n,0,sqrt(0.29))
    z<-c(xx,y)
    v<-ks.test(xx,y)$statistic
    vstar<-numeric(B)
    for(b in 1:B){
      xstar<-sample(z,n,replace = T)
      ystar<-sample(z,n,replace = T)
      vstar[b]<-ks.test(xstar,ystar)$statistic
    }
    test_boot<-as.integer(mean(vstar>=v)<alpha)
    test_boot})
  resul[i]<-mean(unlist(result))
}
resul
without_time <- toc()
stopCluster(cl)

# 3 -> 수기

