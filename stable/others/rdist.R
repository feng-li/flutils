##' Euclidean distance matrix between two matrices
##'
##' Details.
##' @name 
##' @title 
##' @param X 
##' @param Y 
##' @param log 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Thu Dec 23 21:36:38 CET 2010;
##'       Current:       Thu Dec 23 21:36:49 CET 2010.
rdist <- function(X, Y, log = TRUE)
  {
    dim.X <- dim(X)
    dim.Y <- dim(Y)
    if(dim.X[2] != dim.Y[2])
      {
        stop("Input X and Y dimension does not match!")
      }
    sum.X <- matrix(rowSums(X^2), dim.X[1], dim.Y[1])
    sum.Y <- matrix(rowSums(Y^2), dim.X[1], dim.Y[1], byrow = TRUE)
    dist0 <- sum.X + sum.Y - 2*tcrossprod(X, Y)
    dist0[dist0<0] <- 0 # error handling for numerical inconsistent.

    if(log == TRUE) ## return log distance
      {
        out <- 1/2*log(dist0)
      }
    else
      {
        out <- sqrt(dist0)
      }
    
    return(out)
  }
