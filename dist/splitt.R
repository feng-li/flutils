##' Split student-t distribution
##'
##' The split-t distribution.
##' @param x "numeric".
##'
##' @param mu "numeric".
##'
##'        Location parameter. The mode of the density
##'
##' @param df "positive number".
##'
##'        Degrees of freedom.
##'
##' @param phi "positive number".
##'
##'        Scale parameter.
##'
##' @param lmd "positive number".
##'
##'        Skewness parameter. If is 1, reduced to symmetric student t
##'        distribution.
##'
##' @param log "logical".
##'
##'        If the vale is TRUE, return the logarithm from.
##'
##' @return "numeric"
##'
##'         Return the density of split-t distribution.
##'
##' @references Li,  Villani and  Kohn (2010)
##'
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##'
##' @note Created: Sat Jan 05 22:49:34 CET 2013;
##'
##'       Current: Sat Jan 05 22:49:39 CET 2013.
dsplitt <- function(x, mu, df, phi, lmd, log)
{
    I0 <- (x <= mu) # Logical values. 1, if y <= mu; 0, if y >mu.
    I <- (!I0)  # Logical values. 1, if y > mu; 0, if y <= mu.

    sign <- 1*I0 + lmd*I # sign = 1 if y<=mu; sign = lmd.^2 if y>2

    density.log <- (log(2)+ (1+df)/2*(log(df)-log(df+(-mu+x)^2/(phi^2*sign^2)))-
                    log(phi)-log(df)/2-lbeta(df/2,1/2)-log(1+lmd))

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

psplitt <- function(q, mu, df, phi, lmd)
{
    ## CDF for q < = mu part.
    I0 <- as.numeric((q<=mu))
    I <- 1-I0
    sign <- I0*1 + I*lmd
    sign2 <- I0*(-1) + I*1

    A <- df*sign^2*phi^2/(df*sign^2*phi^2+(q-mu)^2)
    BetaRegUpper <- (1- ibeta(x = A,a = df/2,b = 1/2,
                              log = FALSE, reg = TRUE))
    out <- (1/(1+lmd) + sign*sign2/(1+lmd)*BetaRegUpper)
    return(out)
}

qsplitt <- function(p, mu, df, phi, lmd)
{
    n <- length(p)

    mu.long <- p
    mu.long[] <- mu

    df.long <- p
    df.long[] <- df

    phi.long <- p
    phi.long[] <- phi

    lmd.long <- p
    lmd.long[] <- lmd

    I0 <- (p<=(1/(1+lmd.long)))
    I <- (!I0)

    out <- p
    out[] <- NA

    if(any(I0))
    {
        p0 <- p[I0]
        mu0 <- mu.long[I0]
        lmd0 <- lmd.long[I0]
        df0 <- df.long[I0]
        phi0 <- phi.long[I0]

        p0std <- p0*(1+lmd0)/2
        y0std <- qt(p0std, df = df0)

        out0 <- y0std*phi0+mu0
        out[I0] <- out0
    }

    if(any(I))
    {
        p1 <- p[I]
        mu1 <- mu.long[I]
        lmd1 <- lmd.long[I]
        df1 <- df.long[I]
        phi1 <- phi.long[I]

        p1std <- (p1-1/(1+lmd1))*(1+lmd1)/(2*lmd1)+1/2
        y1std <- qt(p1std, df = df1)

        out1 <- y1std*(phi1*lmd1)+mu1
        out[I] <- out1
    }
    return(out)
}

rsplitt <- function(n, mu, df, phi, lmd)
{
    ## Inverse method
    u <- runif(n)
    out <- qsplitt(p = u, mu = mu, df = df, phi = phi, lmd = lmd)
    return(out)
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

    m3 <- (2*h^3+2*h*phi^2*(lmd^2+1)*df/(df-3)
        - 3*h*phi^2*(lmd^3+1)/(lmd+1)*df/(df-2))

    skewness <- m3/var^(3/2)
    return(skewness)
}

splitt.kurtosis <- function(df, phi, lmd)
{
    h <- 2*sqrt(df)*phi*(lmd-1)/((df-1)*beta(df/2, 1/2))
    var <- (1+lmd^3)/(1+lmd)*df/(df-2)*phi^2-h^2

    m4 <- (3*df^2*phi^4*(1+lmd^5)/((1+lmd)*(df-2)*df-4)- 3*h^4
        + 6*h^2*(1+lmd^3)*df*phi^2/((1+lmd)*(df-2))
        - 8*h^2*(lmd^2+1)*df*phi^2/(df-3))

    kurtosis <- m4/var^2-3

    return(kurtosis)
}
