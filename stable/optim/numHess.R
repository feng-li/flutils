numHess <- function(fcn, x, ...) {
  f1 <- fcn
  n <- length(x)
  h <- matrix(0, n, n)
  f2 <- function(z, ...) { numgrad(fcn=f1, z, ...)$g}
  h <- numgrad(fcn=f2, x=x, ...)$g
  return(h)
}
    
