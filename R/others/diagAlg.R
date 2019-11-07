#' Fast matrix algebra with diagonal matrices.
#'
#' This implementation does not involve sparse matrix structure.
#' @param d "vector" Diagonal elements form a diagonal matrix.
#' @param M "matrix" Dense matrix that is conformable to matrix diag(d).
#' @return New conformable matrix.
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
"%d*%" <- dM <- function(d, M)
{
  p <- length(d)
  q <- dim(M)[2]
  D0 <- matrix(d, nrow = p, ncol = q)
  out <- D0*M
  return(out)
}

## X %*% diag(d).
#' @export
"%*d%" <- Md <- function(M, d)
{
  p <- length(d)
  q <- dim(M)[1]
  D0 <- matrix(d, nrow = q, ncol = p, byrow = TRUE)
  out <- M*D0
  return(out)
}

## diag(d) %*% M %*% diag(d) and M is a squared matrix.
#' @export
"%d*d%" <- dMd <- function(d, M)
{
  p <- length(d)
  q <- dim(M)[2]
  D0 <- matrix(d, nrow = p, ncol = q)
  D1 <- t(D0)
  out <- D0*M*D1
  return(out)
}

## t(M) %*% diag(d) %*% N
#' @export
tMdN <- function(M, d, N = M)
{
  p <- length(d)
  q <- dim(N)[2]
  D0 <- matrix(d, nrow = p, ncol = q)
  out <- crossprod(M, D0*N)
  return(out)
}

## Built diag(n) in a much faster way.
#' @export
diag1 <- function(n)
  {
    out <- matrix(0, n, n)
    idx <- seq(1, n^2, n+1)
    out[idx] <- 1
    return(out)
  }
