##' This function is used to compute the incomplete beta function.
##'
##'
##' @title Incomplete beta function
##' @param x a numeric vector in the range [0,1], the point at which the incomplete Beta
##'     function is evaluated.
##' @param a non-negative numeric vectors, the first parameter of the incomplete beta function.
##' @param b non-negative numeric vectors, the second parameter of the incomplete beta function.
##' @param log logical; If TRUE, return the log incomplete beta function.
##' @param reg logical; If TRUE, return the regularized incomplete beta function. Same as
##'     the Mathematica definition.
##'
##' @return a numeric vector.
##' @references \url{http://mathworld.wolfram.com/IncompleteBetaFunction.html}
##'
##' TODO: let -1 < b < oO.
##' @export
ibeta <- function(x, a, b, log = FALSE, reg = FALSE)
{
  HCond <- (all(a >0) & all(b>0))
  if(!HCond)
    {
      stop("Incomplete beta function requires a > 0 and b > 0.")
    }

  ibeta.log <- pbeta(x, a, b,  log = TRUE) + lbeta(a, b)

  if(reg == TRUE)
    {
      out.log <- ibeta.log - lbeta(a = a, b = b)
    }
  else
    {
      out.log <- ibeta.log
    }


  if(log == TRUE)
    {
      out <- out.log
    }
  else
    {
      out <- exp(out.log)
    }

  return(out)
}
