##' Fold a matrix
##'
##' <details>
##' @title
##' @param Mat
##' @param nfolds
##' @param byrow
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Thu Jan 27 20:09:56 CET 2011;
##'       Current:       Thu Jan 27 20:09:59 CET 2011.
##' @export
foldMat <- function(Mat, nfolds, byrow = TRUE)
  {

    dim.x <- dim(Mat)
    mn <- prod(dim.x)

    idx0 <- matrix(1:prod(dim.x), dim.x[1], dim.x[2]) # The original index

    ## if byrow = T
    if(byrow == TRUE)
      {
        nrow1 <- dim.x[1]/nfolds # the new matrix row no.
        ncol1 <- mn/nrow1 # the new matrix col no.

        idx1.tmp <- matrix(idx0[1:nrow1, ], nrow1, ncol1)
        idx2.tmp <- matrix(rep(seq(0, by = nrow1 , length.out = nfolds), each =
                               dim.x[2]), nrow1, ncol1, byrow = TRUE)
        idx <- as.vector(idx1.tmp + idx2.tmp)
        out <- matrix(Mat[idx], nrow1, ncol1)
      }
    else
      {
        nrow1 <- dim.x[1]/nfolds # the new matrix row no.
        ncol1 <- mn/nrow1 # the new matrix col no.

        idx1.tmp <- matrix(idx0[1:nrow1, ], nrow1, ncol1)
        idx2.tmp <- matrix(rep(seq(0, by = nrow1 , length.out = nfolds), each =
                               dim.x[2]), nrow1, ncol1, byrow = TRUE)
        idx <- as.vector(idx1.tmp + idx2.tmp)

        out <- matrix(Mat[idx], nrow1, ncol1)
      }
    return(out)
  }
##----------------------------------------------------------------------------------------
## TESTS: PASSED
##----------------------------------------------------------------------------------------

## p <- 100
## q <- 2

## Mat <- matrix(1:(p^2*q^2), p^2*q, q)
## system.time(cc <- foldMat(Mat, 10, byrow = T))
