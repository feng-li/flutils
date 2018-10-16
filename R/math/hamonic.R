##' Harmonic number
##'
##'
##' @title Harmonic number
##' @param n "non-negative numeric" Non integer is also allowed.
##' @return "numeric"
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Wed May 30 10:51:32 CEST 2012;
##'       Current: Wed May 30 10:51:37 CEST 2012.
##' @export
harmonic <- function(n)
  {
    out <- digamma(n+1) - digamma(1)
    return(out)
  }
