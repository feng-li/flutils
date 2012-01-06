##' Fast diagonal matrix mutiplies a dense matrix.
##'
##' Details.
##' @name 
##' @title 
##' @param D 
##' @param X 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Tue Nov 09 11:12:05 CET 2010;
##'       Current:       Tue Nov 09 11:12:13 CET 2010.
"%d*%" <- diag.prod <- function(D, X) ## D %*% X where D is a diagonal matrix.
{
  p <- dim(D)[1]
  q <- dim(X)[2]
  d <- diag(D)
  D0 <- matrix(d, nrow = p, ncol = q)
  out <- D0*X
  return(out)
}

"%*d%" <- prod.diag <- function(X, D) ## X %*% D where D is a diagonal matrix.
{
  p <- dim(D)[1]
  q <- dim(X)[1]
  d <- diag(D)
  D0 <- matrix(d, nrow = q, ncol = p, byrow = TRUE)
  out <- X*D0
  return(out)
}


"%d*d%" <- diag.prod.diag <- function(D, X) ## D %*% X %*% D where D is a diagonal matrix
  ## and X is a squire matrix.
{
  p <- dim(D)[1]
  q <- dim(X)[2]
  d <- diag(D)
  D0 <- matrix(d, nrow = p, ncol = q)
  D1 <- t(D0)
  #print(D)
  #print(D0)
  #print(X)

  out <- D0*X*D1
  return(out)
}



## "%*d*%" <- prod.diag.prod <- function(X1, D, X2 = X1) ## t(X) %*% D %*% X where D is a diagonal matrix.
## {
##   p <- dim(D)[1]
##   d <- diag(D)
##   D0 <- matrix(d, nrow = p, ncol = p)
##   D1 <- t(D0)
##   out <- D0*X*D1
##   return(out)
## }
