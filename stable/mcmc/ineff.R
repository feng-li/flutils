##' Inefficiency factor of given MCMC chain
##'
##' Just the sum of autocorrelations
##' @title Inefficiency factor
##' @param par "vector" like
##' @return "numeric"
##' @references Geweke 1992
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Sun Sep 02 13:59:26 CEST 2012;
##'       Current: Sun Sep 02 13:59:32 CEST 2012.
ineff <- function(par)
  {
    autocorr <- acf(par, plot = FALSE, type = "correlation")$acf

    ## The inefficiency factor
    ## ineff  = 1 + sum(autocorrelations(lag1 to lag oo))
    out <- 2*sum(autocorr) -1 # acf including lag 0 already.
    return(out)
  }
