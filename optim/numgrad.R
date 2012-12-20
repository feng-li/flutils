numgrad <- function(fcn, x, ...) {
  ## fcn can return a vector, in which case numgrad returns a matrix.
  delta <- 1e-6
  ## delta <- 1e-8
  n <- length(x)
  ## we tolerate x's that may be n x 1, 1 x n, or R vectors (with no dim),
  ## but note that g comes out as n x k matrix regardless.
  tvec <- delta*diag(n)
  f0 <- fcn(x,...)
  k <- length(f0)
  g <- matrix(0,n,k)
  badg <- FALSE
  for (i in 1:n){
    scale <- 1
    tvecv <- tvec[,i]
    if(is.null(dim(x))){
      tvecv <- as.vector(tvecv)
    }else{
      dim(tvecv) <- dim(x)
    }
    g0 <- (fcn(x+scale*tvecv,...) - f0)/(scale*delta)
    if (max(abs(g0))< 1e15){
      g[i, ] <- as.vector(g0)
    }else{
      cat("bad gradient ------------------------\n")
      badg <- TRUE
    }
  }
  return(list(g=g,badg=badg))
}
