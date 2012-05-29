## StdData:
##   Standardize a vector or colums of a matirx

## Description:
##   Standardize a vector or colums of a matrix

## Usage:
##   StdData(X,method)

## Arguments:
##   X:            "Numercial". The data to be standardized.
##   method:       "Numerical". If "stdnorm", X will be standardized with mean
##   zero and standard deviation 1. If "-1to1", X will be restricted to [-1,
##   1].

## Details:
##   "X" should be a numeric matrix or a vector. The standardization methods do _NOT_ apply
##   to tho following situations: 1) there is only one row in the matrix. 2) some colums
##   have constant values.
##   The "method" applies to each colums for a matrix or a vector.

## Value:
##   A matrix or a vector depends on the input X

## Author:
##   Feng Li <Feng.Li@stat.su.se>, Dept. of Statistics, Stockholm University, Sweden.

## Version:
##   First:       Thu Mar  4 13:50:30 CET 2010
##   Current:     Tue May 29 12:46:09 CEST 2012
StdData <- function(X,method)
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

  if(tolower(method)=="-1to1") # restrict to [-1 1]
    {
      max.vec <- apply(X, 2, max)
      min.vec <- apply(X, 2, min)
      max.mat <- matrix(max.vec,X.dim[1],X.dim[2],byrow=TRUE)
      min.mat <- matrix(min.vec,X.dim[1],X.dim[2],byrow=TRUE)
      X.out <- (2*X-max.mat-min.mat)/(max.mat-min.mat)
      config <- list(min = min.vec, max = max.vec, method = method)
    } # if(tolower(method)=="-1to1")

  ## When X was a vector.
  if(X.is.vector==TRUE)
    {
      X.out <- as.vector(X.out)
    }

  out <- list(data = X.out, config = config)
  return(out)
}
