##' Same as expand.grid function but return a matrix.
##'
##' Simple implementation to expand grid
##' @param x1 first grid
##' @param x2 second grid
##' @return matrix
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
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
