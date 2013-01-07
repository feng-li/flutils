##' Split student t distribution
##'
##'
##' @title splitt
##' @param x
##' @param mu
##' @param df
##' @param phi
##' @param lmd
##' @param log
##' @return
##' @references Li Villani Kohn 2010 JSPI
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Sat Jan 05 22:49:34 CET 2013;
##'       Current: Sat Jan 05 22:49:39 CET 2013.
dsplitt <- function(x, mu, df, phi, lmd, log)
  {
    I0 <- (x <= mu) # Logical values. 1, if y <= mu; 0, if y >mu.
    I <- (!I0)  # Logical values. 1, if y > mu; 0, if y <= mu.

    sign <- 1*I0 + lmd*I # sign = 1 if y<=mu; sign = lmd.^2 if y>2

    density.log <- (df/(df+(-mu+x)^2/phi^2/sign^2))^((1+df)/2)/
      phi/sqrt(df)/beta(df/2,1/2)/(1+lmd)*2

    if(log == TRUE)
      {
        out <- density.log
      }
    else
      {
        out <- exp(density.log)
      }
    return(out)
  }

psplitt <- function(x, mu, df, phi, lmd, log)
  {
    ## CDF for y < = mu part.
    I0 <- (x<=mu)
    I <- (!I0)
    sign <- I0*1 + I*lmd
    sign2 <- I0*(-1) + I*1

    A <- df*sign^2*phi^2/(df*sign^2*phi^2+(x-mu)^2)
    BetaRegUpper <- 1- ibeta(x = A,a = df/2,b = 1/2,
                                log = FALSE, reg = TRUE)
    out <- 1/(1+lmd) + sign*sign2/(1+lmd)*BetaRegUpper

    if(log == TRUE)
      {
        stop("log form is not implemented yet!")
      }
    else
      {
        return(out)
      }
  }

splitt.mean <- function(mu, df, phi, lmd)
  {
    h <- 2*sqrt(df)*phi*(lmd-1)/((df-1)*beta(df/2, 1/2))
    mean <- mu + h
    return(mean)
  }

splitt.var <- function(df, phi, lmd)
  {
    h <- 2*sqrt(df)*phi*(lmd-1)/((df-1)*beta(df/2, 1/2))
    var <- (1+lmd^3)/(1+lmd)*df/(df-2)*phi^2-h^2
    return(var)
  }

splitt.skewness <- function(df, phi, lmd)
  {
    h <- 2*sqrt(df)*phi*(lmd-1)/((df-1)*beta(df/2, 1/2))
    var <- (1+lmd^3)/(1+lmd)*df/(df-2)*phi^2-h^2

    m3 <- 2*h^3+2*h*phi^2*(lmd^2+1)*df/(df-3) -
      3*h*phi^2*(lmd^3+1)/(lmd+1)*df/(df-2)

    skewness <- m3/var^(3/2)
    return(skewness)
  }

splitt.kurtosis <- function(df, phi, lmd)
  {
    h <- 2*sqrt(df)*phi*(lmd-1)/((df-1)*beta(df/2, 1/2))
    var <- (1+lmd^3)/(1+lmd)*df/(df-2)*phi^2-h^2

    m4 <- 3*df^2*phi^4*(1+lmd^5)/((1+lmd)*(df-2)*df-4)- 3*h^4 +
      6*h^2*(1+lmd^3)*df*phi^2/((1+lmd)*(df-2))-
        8*h^2*(lmd^2+1)*df*phi^2/(df-3)

    kurtosis <- m4/var^2-3

    return(kurtosis)
  }
