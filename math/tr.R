##' Give the trace of a squire matrix.
##'
##' It is essentially "sum(diag(X))" but faster.
##' @name tr
##' @title trace of a matrix
##' @param X squire matrix.
##' @return scalor
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Wed Dec 29 16:25:04 CET 2010;
##'       Current:       Wed Dec 29 16:25:10 CET 2010.
tr <- function(X)
  {
    nrow0 <- nrow(X)
    ncol0 <- ncol(X)
    
   if(length(X) == 1) # trace of a scalor is itself.
      {
        out <- X
      }
    else if(!is.matrix(X) || nrow0 != ncol0)
      {
        stop("input is not a squire matrix")
      }
    else ## Just sum(diag(X)) but much faster.
      {
        idx <- seq(1, by = nrow0 + 1, length.out = nrow0)
        out <- sum(X[idx])
      }
    return(out)
    
  }
