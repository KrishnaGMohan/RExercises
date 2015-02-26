rm(list=ls())
#-----------------------------
# Learning

(m2 <- matrix(1:20, 4, 5))
lower.tri(m2)
m2[lower.tri(m2)] <- NA
m2




#-----------------------------
rm(list=ls())

#-----------------------------
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

#----------------------------------------------------------------

# 5. Write a function which takes 2 arguments n and k which are positive integers. It should return the n * n
# matrix: 
# [
# k 1 0 0 . . . 0 0
# 1 k 1 0 . . . 0 0
# 0 1 k 1 . . . 0 0
# 0 0 1 k . . . 0 0
# . . . . . . . . .
# 0 0 0 0 . . . k 1
# 0 0 0 0 . . . 1 k
# ]
# Hint: First try to do it for a specific case such as n = 5 and k = 2 on the Command Line.


tmpFn <- function(n,k)
{
  mat <- diag(n) * k
  mat[ abs(row(mat) - col(mat)) == 1] <-1
  mat
}

tmpFn(5,3)

#---------------------------------------------------------------------
# 
# 6. Suppose an angle alpla is given as a positive real number of degrees.
# If 0 <= alpha < 90 then it is quadrant 1. 
# If 90 <= apha < 180 then it is quadrant 2.
# If 180 <= alpha < 270 then it is quadrant 3. 
# If 270 <= alpha < 360 then it is quadrant 4.
# If 360 <= alpha < 450 then it is quadrant 1. And so on.
# Write a function quadrant(alpha) which returns the quadrant of the angle alpha.
# 

quadrant <- function(alpha)
{
    1 + (alpha %% 360) %/% 90 
}

quadrant(900)

#-----------------------------------


rm(list=ls())