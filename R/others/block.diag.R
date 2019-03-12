##' Create a block diagonal matrix.
##'
##' For very large matrix. Consider using .bdiag() function in package "Matrix".
##' @title Block diagonal matrix.
##' @param x "list" contains the block matrices
##' @return "matrix" The block diagonal matrix.
##' @references Modified from R-help list.
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Thu Feb 03 19:16:21 CET 2011;
##'       Current: Sun Mar 04 17:14:51 CET 2012.
##' @export
block.diag <- function(x)
  {
    if(!is.list(x))
      {
        stop("input is not a list")
      }

    n <- length(x)
    if(n==0) return(NULL)

    x <- lapply(x, function(y) if(length(y)) as.matrix(y) else
                stop("Zero-length component in x"))
    d <- array(unlist(lapply(x, dim)), c(2, n))
    rr <- d[1,]
    cc <- d[2,]
    rsum <- sum(rr)
    csum <- sum(cc)
    out <- array(0, c(rsum, csum))
    ind <- array(0, c(4, n))
    rcum <- cumsum(rr)
    ccum <- cumsum(cc)
    ind[1,-1] <- rcum[-n]
    ind[2,] <- rcum
        ind[3,-1] <- ccum[-n]
    ind[4,] <- ccum
    imat <- array(1:(rsum * csum), c(rsum, csum))
    iuse <- apply(ind, 2, function(y, imat) imat[(y[1]+1):y[2],
                                                     (y[3]+1):y[4]], imat=imat)
    iuse <- as.vector(unlist(iuse))
    out[iuse] <- unlist(x)


    return(out)
  }

##------------------------------------------------------------------------------
## TESTS: PASSED
##------------------------------------------------------------------------------
## mats <- list(matrix(1:20, 5, 4), matrix(1:12, 4, 3), matrix(1:25, 5,5))
## block.diag(mats)
