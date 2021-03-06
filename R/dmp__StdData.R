#' StdData
#'
#' Standardize a vector or colums of a matrix
#' @param X "Numercial". The data to be standardized. "X" should be a numeric matrix or
#' a vector. The standardization methods do _NOT_ apply to tho following situations: 1)
#' there is only one row in the matrix. 2) some colums have constant values.
#'
#' @param method "Numerical". If "stdnorm", X will be standardized with mean zero and
#' standard deviation 1. If "-1to1", X will be restricted to [-1, 1].  The "method"
#' applies to each colums for a matrix or a vector.
#'
#' @return A matrix or a vector depends on the input X
#'
#'
#' @author Feng Li, Dept. of Statistics, Stockholm University, Sweden.
#' @note First: Thu Mar 4 13:50:30 CET 2010 Current: Sun Dec 21 04:24:52 EST 2014
#' @export
StdData <- function(X, method)
{
  ## if X is a vector,  treat it as a one-column vector.
  if(is.vector(X)==TRUE)
  {
    X <- as.matrix(X)
    X.is.vector <- TRUE
  }
  else
  {
    X.is.vector <- FALSE
  }
  X.dim <- dim(X)

  ## Methods of standardization
  if(tolower(method)=="norm-0-1") # mean 0, sd=1
  {
    mean.vec <- colMeans(X)
    mean.mat <-  matrix(mean.vec,X.dim[1],X.dim[2],byrow=TRUE)
    sd.vec <- apply(X, 2, sd)
    sd.mat <-  matrix(sd.vec, X.dim[1],X.dim[2],byrow=TRUE)

    X.out <- (X-mean.mat)/sd.mat
    config <- list(mean = mean.vec, sd = sd.vec, method = method)
  } # if(tolower(method)=="stdnorm")
  else  if(tolower(method)=="-1to1") # restrict to [-1 1]
  {
    max.vec <- apply(X, 2, max)
    min.vec <- apply(X, 2, min)
    max.mat <- matrix(max.vec,X.dim[1],X.dim[2],byrow=TRUE)
    min.mat <- matrix(min.vec,X.dim[1],X.dim[2],byrow=TRUE)
    X.out <- (2*X-max.mat-min.mat)/(max.mat-min.mat)
    config <- list(min = min.vec, max = max.vec, method = method)
  } # if(tolower(method)=="-1to1")
  else
  {
    stop("No such method found!")
  }

  ## When X was a vector.
  if(X.is.vector==TRUE)
  {
    X.out <- as.vector(X.out)
  }

    if(any(is.na(X.out)))
    {
        warning("NA/NaN occurs, check the data first.")
    }

  out <- list(data = X.out, config = config)
  return(out)
}
