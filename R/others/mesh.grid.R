##' Same as expand.grid function but return a matrix.
##'
##' Details.
##' @name
##' @title
##' @param x1
##' @param x2
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
##' @export
mesh.grid <- function(x1, x2 = x1)
{
  n.grid1 <- length(x2)
  n.grid2 <- length(x1)
  x1.mesh <- rep(x1, times = n.grid1)
  x2.mesh <- rep(x2, each = n.grid2)
  x.mesh <- cbind(x1.mesh, x2.mesh)
  return(x.mesh)
}
