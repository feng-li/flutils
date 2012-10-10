##' Generate random variable from mixture normal distribution  
##'
##' <details>
##' @title 
##' @param n "integer",  numbers of samples to be generated 
##' @param means "list" mean value within each component
##' @param sigmas "list" variance covariance matrix with in each component 
##' @param weights "list" weights in each component
##' @param ncomp "scalar" numbers of components 
##' @return "matrix" 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Fri Apr 22 18:50:11 CEST 2011;
##'       Current:       Fri Apr 22 18:50:20 CEST 2011.
##' DEPENDS: rmvnorm
##' TODO: remove the loop
rmixnorm <- function(n, means, sigmas, weights)
  {
    ndim <- dim(means)[1] # dim of matrix
    out <- matrix(NA, n, ndim)
    
    idx <- multinormial <- rmultinom(n = n, 1, prob = weights)
    
    for(i in 1:n)
      {
        which.comp <- which(idx[, i] == 1)

        out[i, ] <- rmvnorm(n = 1, mean = means[, which.comp], sigma = matrix(sigmas[, ,
                                                                 which.comp], ndim))   
      }
    return(out)
  }
