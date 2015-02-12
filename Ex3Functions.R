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


day = 27
month = 2
year = 1997

k <- day
k
y <- year %% 100
y
c <- year %/% 100
c
m <- ((month - 3) %% 12) + 1
m

floor(2.6 * m - 0.2)

dow <- as.integer(( floor(2.6 * m - 0.26) + k + y + (y%/%4) + (c%/%4) - 2*c)%%7)



weekday <- function(day, month, year)
{
  k <- day
  y <- year %% 100
  c <- year %/% 100
  m <- ((month - 3) %% 12) + 1
  
  dow <- as.integer(( floor(2.6 * m - 0.26) + k + y + (y%/%4) + (c%/%4) - 2*c)%%7)
  c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")[dow +1]
}

weekday(10,2,2015)
weekday(10,7,1969)

weekday( c(27,18,21), c(2,2,1), c(1997,1940,1963) )







testLoop <- function(n)
{
  if (n < 4) stop("The argument n must be an integer which is at least 4.\n")
  x <- rep(NA, n-1)
  x[1] <- 1
  x[2] <- 2
  
  for (j in (3:(n-1)))
  {
    x[j] <- x[j-1] + 2/x[j-1]
  }  
  x
}

testLoop(8)




testLoop2 <- function(yVec)
{
    sum(exp(seq(along=yVec)))
}

yVec <- vector(mode="numeric", length=0)
#yVec <- c(4,1,7,2)
seq(along=yVec)
length(yVec)
testLoop2(yVec)





quadmap <- function( start, rho, niter)
{
  x <- rep(NA, niter)
  x[1] <- start
  for (k in 2:niter)
  {
    x[k] <- rho * x[k - 1] * (1 - x[k - 1])
  }
  x
}

tmp <- quadmap(start=0.95, rho=2, niter=500)
plot(tmp, type = "l")

tmp <- quadmap(start=0.95, rho=2.99, niter=500)
plot(tmp, type = "l")
plot(tmp[300:500], type = "l")



quadmap2 <- function( start, rho)
{
  niter <- 1
  x <- c(start, rho * start * (1 - start))
  
  while ( abs(x[2] - x[1]) >= 0.02 )
  {
    x[1] <- x[2]
    x[2] = rho * x[1] * (1 - x[1])
    niter <- niter + 1
  }
  niter
}

quadmap2(start = 0.95, rho = 2.99)

tmp





x <- seq(2,56,by=3)

tmpFn <- function(xVec)
{
  r <- rep(NA, 2)

  r[1] <- sum((xVec[2:length(xVec)] - mean(xVec)) * (xVec[1:(length(xVec)-1)] - mean(xVec)))/sum((xVec - mean(xVec)) ^ 2)  
  r[2] <- sum((xVec[3:length(xVec)] - mean(xVec)) * (xVec[1:(length(xVec)-2)] - mean(xVec)))/sum((xVec - mean(xVec)) ^ 2)   
  r
}

tmpFn(xVec)



tmpAcf <- function(xVec)
{
  xc <- xVec - mean(xVec)
  denom <- sum(xc^2)
  n <- length(x)
  r1 <- sum( xc[2:n] * xc[1:(n-1)] )/denom
  r2 <- sum( xc[3:n] * xc[1:(n-2)] )/denom
  list(r1 = r1, r2 = r2)
}

tmpAcf <- function(x, k)
{
  xc <- x - mean(x)
  denom <- sum(xc^2)
  n <- length(x)
  tmpFn <- function(j){ sum( xc[(j+1):n] * xc[1:(n-j)] )/denom }
  c(1, sapply(1:k, tmpFn))
}