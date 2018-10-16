##' Description
##'
##' Details.
##' @name
##' @title
##' @param X
##' @param diag
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
##' @export
vech <- function(X, diag = TRUE)
{
  out <- matrix(X[as.vector(lower.tri(X, diag = diag))])
  return(out)
}
