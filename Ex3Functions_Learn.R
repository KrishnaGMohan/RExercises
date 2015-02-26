rm(list=ls())

# 1. 
# (a) Write functions tmpFn1 and tmpFn2 such that if xVec is the vector (x1, 
# x2,..., xn), then tmpFn1(xVec) returns the vector ( x1, x2^2,  ..., xn^n 
# ) and tmpFn2(xVec) returns the vector ( x1, x2^2/2 , ... , xn^n/n ) .

set.seed(75)
xVec <- sample(1:9,size=6,replace=T )

tmpFn1 <- function(xVec)
{
  xVec^seq(along=xVec)
}

xVec
tmpFn1(xVec)


tmpFn2 <- function(xVec)
{
  (xVec^seq(along=xVec))/seq(along=xVec)
}

xVec
tmpFn2(xVec)


# 1. (b) 
# Now write a function tmpFn3 which takes 2 arguments x and n where x is a
# single number and n is a strictly positive integer. The function should return
# the value of 1 + x/1 + x^2/2 + x^3/3 + ... + x^n/n

tmpFn3 <- function(x, n)
{
  1 + sum(x^(1:n)/(1:n))
}

tmpFn3(1,3)


#-------------------------------------------------------------
# 2. Write a function tmpFn(xVec) such that if xVec is the 
# vector x = (x1,..., xn) 
# then tmpFn(xVec) returns the vector of moving averages: (x1 + x2 + x3)/3 ,
# (x2 + x3 + x4)/3 , ...,  (xn−2 + xn−1 + xn)/3 Try out your function; for
# example, try tmpFn( c(1:5,6:1) ).

tmpFn <- function(xVec)
{
  n <- length(xVec)
  zVec <- (xVec[1:(n-2)] + xVec[2:(n-1)] + xVec[3:n])/3
  zVec
}

tmpFn( c(1:5,6:1) )

# -------------------------------------------------------

# 3. Consider the continuous function 

#  f(x) = x^2 + 2x + 3   if x < 0  
#       = x + 3          if 0 < x < 2 
#       = x^2 + 4x - 7   if 2 <= x. 
# Write a function tmpFn which takes a single
# argument xVec. The function should return the vector of values 
# of the function f(x) evaluated at the values in xVec. 
# Hence plot the function f(x) for -3 < x < 3.


f<- function(x)
{
  ifelse(x < 0, x^2 + 2*x + 3, ifelse(x >= 2, x^2 +4*x -7, x+3))
}

x <- seq(-3,3,len=100)
plot(x, f(x), "l")

#----------------------------------------------------------

# 4. Write a function which takes a single argument which is a matrix. 
# The function should return a matrix # which is the same as the function 
# argument but every odd number is doubled.
# Hence the result of using the function on the matrix
# [ 
#   1   1   3
#   5   2   6
#   -2  -1  -3
# ]
# should be: 
# [ 
#    2    2   6
#   10    2   6
#   -2   -2   -6
# ]
# Hint: First try this for a specific matrix on the Command Line.

tmpFn <- function(mat)
{
  ifelse(mat %% 2 == 1, mat *2, mat)
}

mat <- matrix(c(1,1,3, 5,2,6, -2,-1,-3), nrow = 3, ncol = 3, byrow = T)

mat
tmpFn(mat)



#--------------------------------------------------------------
rm(list=ls())
