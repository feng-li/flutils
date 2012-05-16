##' The gradient for the mean function in the GLM framework.
##'
##' <details>
##' @title <short tile>
##' @param par
##' @param link "character" Type of link function
##' @param extArgs "list" the external arguments need to pass to the function.
##' @param X "matrix" The covariates matrix.
##' @return "matrix" of the same dimension as the linear predictor
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Thu Nov 24 11:46:32 CET 2011;
##'       Current: Thu Nov 24 11:46:39 CET 2011.
parMeanFunGrad <- function(par, link, extArgs)
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
    else if(tolower(link) == "glogit")
      {
        a <- extArgs$a
        b <- extArgs$b
        exp.linPred <- exp(linpred)

        ## The gradients for all three parameters
        out.lin <- (b-a)*exp.linPred/(1+exp.linPred)^2
        ## out.a <- 1/(1+exp.linPred)
        ## out.b <- 1/(1+1/exp.linPred)
        out <- out.lin
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
