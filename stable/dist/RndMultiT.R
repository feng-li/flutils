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
