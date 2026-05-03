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

#' @rdname grapes-d-times-grapes
#' @export
"%*d%" <- Md <- function(M, d)
{
  p <- length(d)
  q <- dim(M)[1]
  D0 <- matrix(d, nrow = q, ncol = p, byrow = TRUE)
  out <- M*D0
  return(out)
}

#' @rdname grapes-d-times-grapes
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

#' @rdname grapes-d-times-grapes
#' @param N A dense matrix used by `tMdN()`.
#' @export
tMdN <- function(M, d, N = M)
{
  p <- length(d)
  q <- dim(N)[2]
  D0 <- matrix(d, nrow = p, ncol = q)
  out <- crossprod(M, D0*N)
  return(out)
}

#' Fast identity matrix
#'
#' @param n A positive integer giving the matrix dimension.
#' @return An `n` by `n` identity matrix.
#' @export
diag1 <- function(n)
  {
    out <- matrix(0, n, n)
    idx <- seq(1, n^2, n+1)
    out[idx] <- 1
    return(out)
  }
