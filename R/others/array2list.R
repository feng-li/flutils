##' Convert an array(or matrix) to a list
##'
##' Also works with matrix.
##' @title
##' @param X "array"
##' @param MARGIN "integer"
##' @return "list"
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
##' @export
array2list <- function(X, MARGIN)
  {
    dim4X <- dim(X)
    if(length(dim4X) >=  3) ## Array
      {
        dim4list <- dim4X[-MARGIN]
      }
    else if(length(dim4X) == 2)
      {
       dim4list <- matrix(c(1, dim4X[2], dim4X[1], 1), 2, 2)[, MARGIN]
      }
    else
      {
        stop("X must be dimension attributed!")
      }

    out <- lapply(apply(X, MARGIN = MARGIN, list),
                  function(x, dim4list) array(unlist(x), dim4list), dim4list = dim4list)
    return(out)
  }
##----------------------------------------------------------------------------------------
## TESTS: PASSED
##----------------------------------------------------------------------------------------
## p <- 2
## q <- 100
## q_i <- 30
## a <- array(1, c(p*q, q_i, q_i))
## system.time(array2list(a))
