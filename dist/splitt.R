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

    Sign <- 1*I0 + lmd^2*I # Sign = 1 if y<=mu; Sign = lmd.^2 if y>2

    density.log <- (df/(df+(-mu+x)^2/phi^2/Sign))^((1+df)/2)/
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

    out <- x
    out[0:length(x)] <- NA

    if(length(I0) != 0L)
      {
        A0 = df[I0]/(df[I0]+(x[I0]-mu[I0])^2/phi[I0]^2)

        BetaRegularized0 = 1- ibeta(x = A0,a = df(I0)/2,b = 1/2,
          log = FALSE, reg = TRUE)

        density0 = (1 - BetaRegularized0)/(1+lmd[I0])
        out[I0]= density0
      }

    if(length(I) != 0L)
      {
        ## CDF for y > mu part.
        A = df[I]/(df[I]+(x[I]-mu[I])^2/phi[I]^2/lmd[I]^2)
        BetaRegularized = 1- ibeta(x = A,a = df(I)/2,b = 1/2,
          log = FALSE, reg = TRUE)

        density1 = (1 - lmd[I] + lmd[I]*(1+ BetaRegularized))/(1+lmd[I])

        out[I]=density1
      }


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
