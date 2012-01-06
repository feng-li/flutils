##' Check if a square matrix is computational singular
##'
##' <details>
##' @title 
##' @param X 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
is.singular <- function(X, tol = .Machine$double.eps*1e3)
  {
    rcond.out <- rcond(X)
    if(rcond.out < tol)
      {
        singular = TRUE
      }
    else
      {
        singular  = FALSE
      }
    return(singular)
  }
