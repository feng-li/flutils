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
##   Feng Li <feng.li@cufe.edu.cn>, Dept. of Statistics, Stockholm University, Sweden.

## License: GPL(>=2)

## Version:
##   First:       Tue Mar  9 16:00:06 CET 2010
##   Current:     Wed Mar 14 19:02:51 CET 2012

#' @export
K <- function(m,n)
{
  x <- matrix(0,m*n,m*n)
  m0 <- 1:(m*n)
  n0 <- as.vector(t(matrix(m0,m,n)))

  arr.ind <- cbind(m0, n0)
  dims <- c(m*n,m*n)
  arr.indMat <- matrix(arr.ind, ncol = length(dims))
  idx1 <- cumprod(dims[-length(dims)])
  idx2 <- arr.indMat[, -1, drop = FALSE]-1
  idxRaw <- rowSums(idx1*idx2) + arr.indMat[, 1]

  x[idxRaw] <- 1

  return(x)
}
