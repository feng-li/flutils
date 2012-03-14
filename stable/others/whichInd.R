##' Find the raw indices for given indices in array form. 
##'
##' This is exactly the inverse function of `arrayInd()'
##' @title Raw indices finder for array.
##' @param arr.ind "matrix or vector"
##'        When it is a matrix, the length of columns should be the as the
##' length of the "dims". When it is a vector, the argument length should be
##' the same as the length of "dims".
##' @param dims "vector" The dimension of the array.
##' @return "vector" The raw indices.
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Wed Mar 14 18:03:01 CET 2012;
##'       Current: Wed Mar 14 18:03:09 CET 2012.
whichInd <- function(arr.ind, dims)
  {
    arr.indMat <- matrix(arr.ind, ncol = length(dims))
    idx1 <- cumprod(dims[-length(dims)])
    
    idx2 <- arr.indMat[, -1, drop = FALSE]-1
    out <- rowSums(idx1*idx2) + arr.indMat[, 1]
    return(out)
  }
