## K:
##   Make a commutation matrix

## Description:
##   A fast way to make a commutation matrix.
   
## Usage:
##   K(m, n)

## Arguments:
##   m:       "Integer".  
##   n:       "Integer". 

## Details:
##   Both 'm' and 'n' shoud be positive integers
  
## Value:
##   A matrix with dimension of "mn--by--mn".

## Author:
##   Feng Li <Feng.Li@stat.su.se>, Dept. of Statistics, Stockholm University, Sweden.

## License: GPL(>=2)

## Version:
##   First:       Tue Mar  9 16:00:06 CET 2010
##   Current:     Tue Mar  9 16:00:15 CET 2010

K <- function(m,n)
{
  x <- matrix(0,m*n,m*n)
  m0 <- 1:(m*n)
  n0 <- as.vector(t(matrix(m0,m,n)))
  for (i in 1:(m*n)) # TODO: remove the loops: Email from "Yves Rosseel"
    {
      x[m0[i],n0[i]] <- 1
    }
  return(x)
}
