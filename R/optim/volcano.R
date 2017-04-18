volcano <- function(x) {
  rho <- sqrt(sum(x))
  f <- rho^2*exp(-rho)+.02*x[1]/(1+.001*x[1]^2)
  return(-f)
}
