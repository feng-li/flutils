##' Density for the inverse wishart distribution
##'
##' Details.
##' @name 
##' @title 
##' @param X 
##' @param df 
##' @param V Location matrix 
##' @param log 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
diwishart <- function(X, df, V, log = TRUE)
{

 ## TODO: Condition check...
  
  p <- dim(V)[1]
  out <- (-df*p/2)*log(2)-mvgamma(p = p, x = df/2, log = TRUE)+
              df/2*determinant(V)$modulus[1] -
                (df+p+1)/2*determinant(X)$modulus[1] -
                  1/2*sum(diag(solve(X)%*%V))
  if(log == TRUE) # Give the log density
    { return(out) }
  else # Return the usual one
    { return(exp(out)) }
}
