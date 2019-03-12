##' Multivariate gamma function
##'
##' The multivariate gamma function is computed by using it's recursive
##' definition or product form. Use apply() on top of the mvgamma function if p
##' is also a vector.
##' @param p "integer".  The dimension of the multivariate gamma function.
##' @param x "vector"
##'         A vector or sclor can be used. See also gamma() function.
##' @param log "logical"
##'         If TRUE, logarithm of multivariate gamma function will be returned.
##' @return Same length as "x" has.
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Initial: Mon Sep 20 23:54:20 CEST 2010;
##'       Current: Tue Jan 15 18:56:48 CET 2013.
##'
##' TODO: Allow p be a vector of same length of x
##' @export
mvgamma <- function(p, x, log = TRUE)
{

  n <- length(x)
  x.mat <- matrix(x, n, p)
  p.mat <- matrix(1:p, n, p, byrow = TRUE)
  g.sum <- apply(lgamma(x.mat+(1-p.mat)/2), 1, sum)

  out.log <- p*(p-1)/4*log(pi) + g.sum

  ## if(p>1)
  ##   {
  ##     out <- (p-1)/2*log(pi) + mvgamma(p-1, x, log = TRUE) + lgamma(x+(1-p)/2)
  ##   }
  ## else if (p == 1)
  ##   {
  ##     out <- lgamma(x)
  ##   }
  ## else stop("The integer p must exceed 0.")

  if(log == TRUE) # return the log form of multivariate gamma function
    {
      return(out.log)
    }
  else # return the multivariate gamma function
    {
      return(exp(out.log))
    }
}
