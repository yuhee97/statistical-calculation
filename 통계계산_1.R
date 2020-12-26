# 1
# a
rep("a", 6)

# b
seq(1:99)

# c
rep(1:3, each=3)

# d
c(1, 2, 3, 4, 5, 4, 3, 2, 1)

# e
c(1, 3, 5, 8, 13, 21, 34)

# f
rep(1/1:10)

# 2 -> 수기

# 3
# a
names(uspop) <- seq(1790, 1970, by=10)

# b
uspop_d <- diff(uspop)
names(uspop_d) <- seq(1800, 1970, by=10)
max(uspop_d)

# 4
head(sort(islands, decreasing = TRUE), 7) 

# 5
# a
normal <- function(x){
  return(1/sqrt(2*pi*5)*exp(-(x-10)^2/(2*5)))
}

# b
c(normal(seq(5, 15, by=0.5)))

#c
x <- seq(5, 15, by=0.5)
y <- c(normal(seq(5, 15, by=0.5)))
plot(x, y, type="l", col='red')

# 6
iris_setosa <- subset(iris, Species=="setosa")
attach(iris_setosa)
Petal <- Petal.Length + Petal.Width
Sepal <- Sepal.Length + Sepal.Width
summary(Petal)
summary(Sepal)

# 7
# a
BMI <- function(k,m){
  bmi <- k/m^2
  return(bmi)
}
kg <- c(60, 72, 57, 100, 95, 72)
m <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
people <- data.frame(kg,m)
people$BMI <- BMI(people$kg, people$m)

# b 
people$BMI[which(people$BMI<=25)] = 0
people$BMI[which(people$BMI>25)] = 1