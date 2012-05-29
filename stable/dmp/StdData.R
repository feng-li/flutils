## StdData:
##   Standardize a vector or colums of a matirx

## Description:
##   Standardize a vector or colums of a matrix
   
## Usage:
##   StdData(X,method)

## Arguments:
##   X:            "Numercial". The data to be standardized. 
##   method:       "Numerical". If "stdnorm", X will be standardized with mean zero and standard deviation 1. If "-1to1", X will be restricted to [-1, 1].

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
##   Current:     Thu Mar  4 13:50:38 CET 2010


StdData <- function(X,method)
{
   X.is.vector <- FALSE
   if(is.vector(X)==TRUE)
     {
      X <- as.matrix(X)
      X.is.vector <- TRUE
      X.const <- any(sd(X)==0)
     }
   if(is.numeric(X) == FALSE || is.matrix(X)==FALSE)
     {stop("X (numeric) must be either a matrix or a vector.")}
   X.dim <- dim(X)
   if(X.dim[1]==1 || any(sd(X)==0)==TRUE )
     {stop("A matrix with one row or constant colums does not work.")}

   X.dim <- dim(X)
   if(tolower(method)=="norm-0-1") # mean 0, sd=1
     {
       X.colMeans <-  matrix(colMeans(X),X.dim[1],X.dim[2],byrow=TRUE)
       X.sd <-  matrix(sd(X),X.dim[1],X.dim[2],byrow=TRUE)
       X.out <- (X-X.colMeans)/X.sd  
     } # if(tolower(method)=="stdnorm")

   if(tolower(method)=="-1to1") # restrict to [-1 1]
     {
       X.max <- matrix(apply(X,2,max),X.dim[1],X.dim[2],byrow=TRUE)
       X.min <- matrix(apply(X,2,min),X.dim[1],X.dim[2],byrow=TRUE)
       X.out <- (2*X-X.max-X.min)/(X.max-X.min)  
     } # if(tolower(method)=="-1to1")

   if(X.is.vector==TRUE)
     {return(as.vector(X.out))}
   else
     {return(X.out)}
   
}
