##' Make a vech vector from a full matrix
##'
##' This function create a vech vector
##' @param X Full matrix
##' @param diag TRUE or FALSE
##' @return vector
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @export
vech <- function(X, diag = TRUE)
{
  out <- matrix(X[as.vector(lower.tri(X, diag = diag))])
  return(out)
}
