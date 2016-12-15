##' Inefficiency factor of given MCMC chain
##'
##' Just the sum of autocorrelations
##' @title Inefficiency factor
##' @param par "vector" like
##' @return "numeric"
##' @references Geweke 1992
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Sun Sep 02 13:59:26 CEST 2012; Current: Tue Aug 11 16:17:18 CST 2015.
ineff <- function(par)
{
    if(any(is.na(par)))
    {
        out <- NA
    }
    else
    {
        autocorr <- acf(par, plot = FALSE, type = "correlation")$acf

        ## The inefficiency factor
        ## ineff  = 1 + 2*sum(autocorrelations(lag1 to lag oo))
        out.init <- 2*sum(autocorr) -1 # acf including lag 0 already.

        out <- out.init
        if(is.na(out.init))
        {
            out <- Inf # Very high correlation indicating high inefficiency.
        }
        else if(out.init < 0)
        {
            ## Inefficiency factor will not valid if it is negative, probably too short chain.
            out <- NaN
        }
    }
    return(out)
}
