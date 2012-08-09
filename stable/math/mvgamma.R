##' Multivariate gamma function
##'
##' The multivariate gamma function is computed by using it's recursive definition.
##' @name mvgamma 
##' @title Multivariate gamma function
##' @param p "integer".
##'         The dimension of the multivariate gamma function.
##' @param x "vector"
##'         A vector or sclor can be used. See also gamma() function.
##' @param log "logical"
##'         If TRUE, logarithm of multivariate gamma function will be returned.
##' @return Same length as "x" has. 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Mon Sep 20 23:54:20 CEST 2010;
##'       Current:       Mon Sep 20 23:54:28 CEST 2010.
mvgamma <- function(p, x, log = TRUE)
{
  if(p>1)
    {
      out <- (p-1)/2*log(pi) + mvgamma(p-1, x, log = TRUE) + lgamma(x+(1-p)/2) 
    }
  else if (p == 1)
    {
      out <- lgamma(x) 
    }
  else stop("The integer p must exceed 0.")
  
  if(log == TRUE) # return the log form of multivariate gamma function
    {
      return(out)
    }
  else if(log  == FALSE) # return the multivariate gamma function
    {
      return(exp(out))
    }
  else stop("Wrong argument input!") 
}
  
