#' Commutation matrix
#'
#' Construct the `mn` by `mn` commutation matrix.
#'
#' @param m,n Positive integer matrix dimensions.
#' @return A commutation matrix with dimension `m * n` by `m * n`.
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
