## K.X:
##   Multiplication of a commutation matrix (K) with a dense matrix (X)

## Description:
##   A fast way to make a multiplication of a commutation matrix (K)
##   with a dense matrix (X)

## Usage:
##   K.X(m, n, X, t)

## Arguments:
##   m:       "Integer". Parameter of the commutation matrix "K"
##   n:       "Integer". Parameter of the commutation matrix "K"
##   X:       "Matrix".  A matrix to be multiplied.
##   t:       "Logical". If FALSE, do K%*%X; else, do X%*%K.

## Details:
##   Both 'm' and 'n' should be positive integers. The dimensions should be comfortable
##   As usual matrices multiplication.

## Value:
##   A matrix with dimension depends on "K" and "X".

## Author:
##   Feng Li <feng.li@cufe.edu.cn>, Dept. of Statistics, Stockholm University, Sweden.

## License: GPL(>=2)

## Version:
##   First:       Wed Mar 10 14:03:31 CET 2010
##   Current:     Wed Mar 10 14:03:39 CET 2010
##' @export
K.X <- function(m,n,X,t)
{
  if(t==FALSE)
   {
    if(dim(X)[1]!=(m*n))
      {stop("The dimensions of (K %*% X) are not conformable.")}
     key <- as.vector(t(matrix(c(1:(m*n)),m,n)))
     X <- X[key,, drop = FALSE]
    }
  else
    {
     if(dim(X)[2]!=(m*n))
      {stop("The dimensions of (X %*% K) are not conformable")}
      key <- as.vector(t(matrix(c(1:(m*n)),n,m)))
      X<- X[,key, drop = FALSE]
    }
  return(X)
}
