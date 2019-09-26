##' Convert from the vech vector to the full matrix.
##'
##'
##' Vech to matrix
##' @param vech Vech vector
##' @param diag TRUE or FALSE
##' @return Full matrix
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @export
vech2m <- function(vech, diag = TRUE)
{
  c <- length(vech)
  p <- (1 - 2*diag +sqrt(8*c+1))/2 # n^2 -n  =  2c; or n^2 + n  =  2c
  if(!identical(p,  round(p)))
    {
      stop ("Input vector length not match with the length of matrix triangular.")
    }
  out <- matrix(1, p, p)
  idx.lower <- as.vector(lower.tri(out, diag = diag))
  out[idx.lower] <- vech
  idx.upper <- as.vector(upper.tri(out, diag = diag))
  out.t <- t(out)
  out[idx.upper] <- out.t[idx.upper]
  return(out)
}
