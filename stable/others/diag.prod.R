##' Fast diagonal matrix mutiplies a dense matrix.
##'
##' Details.
##' @name 
##' @title 
##' @param d 
##' @param M 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Tue Nov 09 11:12:05 CET 2010;
##'       Current:       Tue Nov 09 11:12:13 CET 2010.

## diag(d) %*% M .
"%d*%" <- dM <- function(d, M) 
{
  p <- length(d)
  q <- dim(M)[2]
  D0 <- matrix(d, nrow = p, ncol = q)
  out <- D0*M
  return(out)
}

## X %*% diag(d).
"%*d%" <- Md <- function(M, d) 
{
  p <- lengh(d)
  q <- dim(M)[1]
  D0 <- matrix(d, nrow = q, ncol = p, byrow = TRUE)
  out <- M*D0
  return(out)
}

## diag(d) %*% M %*% diag(d) and M is a squared matrix.
"%d*d%" <- dMd <- function(d, M) 
{
  p <- length(d)
  q <- dim(M)[2]
  D0 <- matrix(d, nrow = p, ncol = q)
  D1 <- t(D0)

  out <- D0*M*D1
  return(out)
}

## M %*% diag(d) %*% N
tMdN <- function(M, d, N = M) 
{
  p <- length(d)
  q <- dim(N)[2]
  D0 <- matrix(d, nrow = p, ncol = q)
  out <- crossprod(M, D0*N)
  return(out)
}
