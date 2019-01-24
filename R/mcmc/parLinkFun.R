##' Transform the mean function in the GLM framework.
##'
##' <details>
##' @title <short tile>
##' @param mu
##' @param linkArgs
##' @return "one-col-matrix" of the same dimension as the linear predictor
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Thu May 10 13:09:08 CEST 2012;
##'       Current: Thu May 10 13:09:14 CEST 2012.
##' @export
parLinkFun <- function(mu, linkArgs)
{
    ## Input each observation x'b  -> l(phi) = x'b -> phi
    ## output: The linear predictor linPred =  X %*% beta

    ## Debugging symbol: if the warning should be printed out immediately.
    immediate. <- FALSE

    link <- linkArgs[["type"]]

    if(tolower(link) %in% "identity")
    {
        out <- mu
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
        out <- log(mu-a)
    }
    else if(tolower(link) %in% c("glogit", "logit"))
    {
        ## The generalized logit link (logit as the special case)
        ## The logit link is a special case when a  =  0 and b  =  1.
        if(tolower(link) == "logit")
        {
            a <- 0
            b <- 1
        }
        else
        {
            a <- linkArgs$a
            b <- linkArgs$b
        }


        ## The output
        out <- log(mu-a) - log(b-mu)
        ## if(any(is.na(out))) browser()

    }
    else
    {
        stop("This link function is not implemented yet!")
    }


    ## Check of unexpected output
    if(any(is.na(out)) || any(is.infinite(out)))
    {
        stop("NA/Inf occurred. Mean function should fall into the boundary of link function strictly.")
    }

    return(out)
}
