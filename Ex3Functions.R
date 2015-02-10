rm(list=ls())
set.seed(75)
xVec<-sample(0:9,size=10,replace=T)



tmpFn1 <- function(x)
{  
  x^(1:length(x))
}


tmpFn2 <- function(x)
{
  n <- length(x)
  (x^(1:n))/(1:n)
}

tmpFn <- function(x)
{
  n<-length(x)-2
  (x[1:n] + x[2:(n+1)] + x[3:(n+2)])/3
}

tmpFn(c(1:5,6:1))




tmpFn <- function(x)
{
  ifelse(x < 0, x^2 + 2*x + 3, ifelse(x >=0 & x < 2, x+3, x^2 + 4*x - 7))
}
tmp <- seq(-3,3,len=100)
plot(tmp, tmpFn(tmp), type="l")

tmpFn <- function(x)
{
  ifelse(x < 0, x^2 + 2*x + 3, ifelse(x < 2, x+3, x^2 + 4*x - 7))
}
tmp <- seq(-3,3,len=100)
plot(tmp, tmpFn(tmp), type="l")





evenFn <- function(mat)
{
  ifelse(mat %% 2 == 1, mat * 2, mat)
}
evenFn(mat)


tmpFn <- function(mat)
{
  mat[mat%%2 == 1] <- 2 * mat[mat%%2 == 1]
  #mat
}
evenFn(mat)
mat

tmpFn <- function(n,k)
{
  m <- matrix(0, nrow=n, ncol=n)
  m[ row(m) == col(m) ] <- k
  m[ abs( row(m) - col(m) ) == 1 ] <- 1
  m
}  

tmpFn <- function(n,k)
{
  m <- diag(k, nrow=n, ncol=n)
  m[ abs( row(m) - col(m) ) == 1 ] <- 1
  m
}  
tmpFn(5,2)
tmpFn(6,7)

quadrant <- function(alpha)
{
  as.integer(( alpha %% 360) / 90) +1
}

m <- seq(0,430,by=10)
matrix(c(m,quadrant(m)),ncol=2,byrow=F)


