##' This function is used to compute the incomplete beta function.
##'
##'
##' @title Incomplete beta function
##' @param x
##' @param a
##' @param b
##' @param log
##' @param reg If TRUE,  return the regularized incomplete beta function. Same
##' as the Mathematica definition.
##'
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Wed Oct 26 18:39:12 CEST 2011; Current: Wed Sep 30 16:37:49 CST 2015.
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
