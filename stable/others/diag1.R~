##' Create diagonal identity matrix.
##'
##' much faster than diag(n)
##' @title 
##' @param n 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
diag1 <- function(n)
  {
    out <- matrix(0, n, n)
    idx <- seq(1, n^2, n+1)
    out[idx] <- 1
    return(out)
  }
