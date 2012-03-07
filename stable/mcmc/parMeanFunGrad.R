##' The gradient for the mean function in the GLM framework.
##'
##' <details>
##' @title <short tile>
##' @param X "matrix" The covariates matrix.
##' @param beta 
##' @param link "character" Type of link function
##' @return "matrix" of the same dimension as the linear predictor
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Thu Nov 24 11:46:32 CET 2011;
##'       Current: Thu Nov 24 11:46:39 CET 2011.
parMeanFunGrad <- function(par, link)
  {
    ## Input x'b  -> l(phi) = x'b -> phi
    ## NOTE: We want vectorized output, i.e, X is n-by-p,  beta is p-by-1 and
    ## the output is n-by-1. But the appendix is written in scaler form.

    ## The linear predictor eta = x'b
    linpred <- parLinkFun(mu = par, link = link) 

    ## Gradient for different situations 
    if(tolower(link) == "identity")
      {
        out <- linpred
        out[1:length(linpred)] <- 1
      }
    else if(tolower(link) == "log")
      {
        out <- exp(linpred)
      }
    else if(tolower(link) == "logit")
      {
        exp.linPred <- exp(linpred)
        out <- exp.linPred/(1+exp.linPred)^2
      }
    else
      {
        stop("This link function is not implemented yet!")
      }
    return(out)
  }


## parMeanFunGrad <- function(X, beta, link)
##   {
##     ## Input x'b  -> l(phi) = x'b -> phi
##     ## NOTE: We want vectorized output, i.e, X is n-by-p,  beta is p-by-1 and
##     ## the output is n-by-1. But the appendix is written in scaler form.
    
##     if(tolower(link) == "identity")
##       {
##         out <- X
##       }
##     else if(tolower(link) == "log")
##       {
##         linPred <- X %*% beta
##         exp.linPred <- array(exp(linPred), dim(X))
        
##         out <- exp.linPred*X
##       }
##     else if(tolower(link) == "logit")
##       {
##         linPred <- X %*% beta
##         exp.linPred <- array(exp(linPred), dim(X))
##         out <- exp.linPred/(1+exp.linPred)^2*X
##       }
##     else
##       {
##         stop("This link function is not implemented yet!")
##       }
##     return(out)
##   }
