##' This function is used to compute the incomplete beta function.
##'
##' 
##' @title Incomplete beta function
##' @param x 
##' @param a 
##' @param b 
##' @param log 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Wed Oct 26 18:39:12 CEST 2011;
##'       Current: Wed Feb 08 20:08:25 CET 2012.
##' TODO: let -1 < b < oO.
ibeta <- function(x, a, b, log = FALSE)
{
  HCond <- (a >0 && b>0)
  if(!HCond) 
    {
      stop("Incomplete beta function requires a > 0 and b > 0.")
    }
  
  out.log <- pbeta(x, a, b,  log = TRUE) + lbeta(a, b)

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
