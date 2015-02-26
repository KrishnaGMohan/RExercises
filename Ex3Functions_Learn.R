# 1. 
# (a) Write functions tmpFn1 and tmpFn2 such that if xVec is the vector (x1, 
# x2,..., xn), then tmpFn1(xVec) returns the vector ( x1, x2^2,  ..., xn^n 
# ) and tmpFn2(xVec) returns the vector ( x1; x22 2 ; : : : ; xnn n ) .

set.seed(75)
xVec <- sample(1:9,size=6,replace=T )

tmpFn1 <- function(xVec)
{
  xVec^seq(along=xVec)
}

xVec
tmpFn1(xVec)
