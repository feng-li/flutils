##' Transform the mean function in the GLM framework.
##'
##' @title GLM mean function
##' @param X "matrix"
##' @param beta "one-col-matrix"
##' @param linkArgs
##' @param link "character" Type of link function
##' @return "one-col-matrix" of the same dimension as the linear predictor
##' @references NA
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Sun Mar 04 14:45:22 CET 2012; Current: Wed Sep 30 12:34:04 CST 2015.
parMeanFun <- function(X, beta, linkArgs)
{
    ## Input each observation x'b  -> l(phi) = x'b -> phi
    ## We allow for beta to be a matrix of (p-by-lq)
    p <- dim(X)[2]
    beta <- matrix(beta, nrow = p)

    linPred <- X %*% beta # The linear predictor

    link <- linkArgs[["type"]]

    if(tolower(link) %in% "identity")
    {
        out <- linPred
    }
    else if(tolower(link) %in% c("log", "glog"))
    {
        if(tolower(link) == "glog")
        {
            a <- linkArgs$a

            if(length(a) == 0)
            {
                stop("A lower boundary parameter `a` for glog link is expected.")
            }

            b <- linkArgs$b

            if(length(b) == 0)
            {
                b <- Inf
            }
        }
        else
        {
            a <- 0
            b <- Inf
        }

        out <- exp(linPred) + a

        ## Cutoff for very big values that cause numerical instable.
        out.upidx <- (out >= b)
        if(length(out.upidx) > 0)
        {
            out[out.upidx] <- b
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
            b <- linkArgs$b

            if(length(a) == 0)
            {
                stop("A lower boundary parameter `a` for glogit link is expected.")
            }

            if(length(b) == 0)
            {
                stop("An upper boundary parameter `b` for glogit link is expected.")
            }

        }

        ## out <- 1/(1+exp(-linPred))
        out <- a + (b-a)/(1+exp(-linPred))
        ## if(any(is.na(out)) || any(out>= b)) browser()
    }
    else
    {
        stop("This link function is not implemented yet!")
    }

    ## if(any(is.na(out))) browser()
    return(out)
}
