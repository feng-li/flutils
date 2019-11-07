#' Calculate the distance between all rows of X and Y.
#' @title Euclidean distance matrix between two matrices
#' @name  rdist
#' @param X an `m-by-p` numeric matrix.
#' @param Y an `n-by-p` numeric matrix.
#' @param log logical; If TRUE, the log distance matrix is returned.
#' @return an `m-by-n` numeric matrix.
#' @export
rdist <- function(X, Y, log = TRUE)
{
    dim.X <- dim(X)
    dim.Y <- dim(Y)
    if(dim.X[2] != dim.Y[2])
    {
        stop("Input X and Y dimension does not match!")
    }
    sum.X <- matrix(rowSums(X^2), dim.X[1], dim.Y[1])
    sum.Y <- matrix(rowSums(Y^2), dim.X[1], dim.Y[1], byrow = TRUE)
    dist0 <- sum.X + sum.Y - 2*tcrossprod(X, Y)
    dist0[dist0<0] <- 0 # error handling for numerical inconsistent.

    if(log == TRUE) ## return log distance
    {
        out <- 1/2*log(dist0)
    }
    else
    {
        out <- sqrt(dist0)
    }

    return(out)
}
