## ---------------------Introduction and Help---------------------------------------------
## RndMultiT:
##      Random number generator for the k-variate student-t distribution with mean mu,
##      covariance matrix.
##      This function can be replaced with "rmvt" function in "mvtnorm" package.
## Arguments:
##      mu          k x 1
##      Sigma       k x k         Must be positive definite.
##      df          Scalar        >=3
##
## Orignally from Mattias Villani.
## ---------------------The code----------------------------------------------------------

RndMultiT <- function(mu, Sigma, df)
{
  k <- length(mu)
  C <- chol(Sigma)  # This is faster but eigen might be stabler. See MASS::mvrnorm
  z <- matrix(rnorm(k))
  x <- rchisq(1,df)
  T <-mu + C%*%z/sqrt(x/(df-2))
  return(T)
}

## ---------------------End here----------------------------------------------------------



## ---------------------Introducntion and Help--------------------------------------------
## DensMultiT
##      Density (log) for the k-variate student-t distribution with mean mu,
##      covariance matrix Sigma and df degrees of freedom.
##      This function can be replaced with "dmvt" function in "mvtnorm" package.
##
## Arguments:
##      x              k x 1
##      mu             k x 1
##      invSigma       k x k         Inverse of variance. should be positive definite.
##      df             Scalar        >=3
##
## Value(s):
##
## ---------------------The code----------------------------------------------------------

DensMultiT <- function(x,mu,invSigma,df)
{
    k <- length(mu)
    p <- invSigma/(df-2) #FAST!
    LogCt <- lgamma(df/2) - lgamma((df+k)/2) + (k/2)*log(pi) - .5*determinant(p)$modulus[1] #FAST!  log(det(p))

    LogDensity <- -LogCt-(df+k)/2*log(1+t((x-mu))%*%p%*%(x-mu))  ## Fix me: NaN produced
    LogDensity
}

## ---------------------End Here----------------------------------------------------------

