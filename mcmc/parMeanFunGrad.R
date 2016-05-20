##' The gradient for the mean function in the GLM framework.
##'
##' <details>
##' @param par "vector"
##' @param linkArgs "list"
##' @return "matrix" of the same dimension as the linear predictor
##' @references Li 2012
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Thu Nov 24 11:46:32 CET 2011;
##'       Current: Thu Nov 24 11:46:39 CET 2011.
parMeanFunGrad <- function(par, linkArgs)
{
    ## Input x'b  -> l(phi) = x'b -> phi
    ## NOTE: We want vectorized output, i.e, X is n-by-p,  beta is p-by-1 and
    ## the output is n-by-1. But the appendix is written in scaler form.

    ## The linear predictor eta = x'b
    linpred <- parLinkFun(mu = par, linkArgs)
    link <- linkArgs[["type"]]

    ## Gradient for different situations
    if(tolower(link) %in% "identity")
    {
        out <- linpred
        out[1:length(linpred)] <- 1
    }
    else if(tolower(link) %in% c("log", "glog"))
    {
        if(tolower(link) == "glog")
        {
            a <- linkArgs$a
            b <- linkArgs$b
            if(is.null(b)) b <- Inf
        }
        else
        {
            a <- 0
            b <- Inf
        }
        out <- exp(linpred)

        ## Let the gradient be zero after cutoff to stabilize the computation.
        out.upidx <- ((out + a) >=  b)
        if(length(out.upidx) > 0)
        {
            out[out.upidx] <- 0
        }



    }
    else if(tolower(link) %in% c("glogit", "logit"))
    {
        if(tolower(link) == "logit")
        {
            a <- 0
            b <- 1
        }
        else
        {
            a <- linkArgs$a

            if(length(a) == 0)
            {
                stop("A lower boundary parameter `a` for glogit link is expected.")
            }

            b <- linkArgs$b

            if(length(b) == 0)
            {
                b <- Inf
            }

        }

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
