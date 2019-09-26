##' Transform the mean function in the GLM framework.
##'
##' GLM mean function
##' @param X "matrix"
##' @param beta "one-col-matrix"
##' @param linkArgs linkage arguments
##' @param link "character" Type of link function
##' @return "one-col-matrix" of the same dimension as the linear predictor
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @export
parMeanFun <- function(X, beta, linkArgs)
{
    ## Input each observation x'b  -> l(phi) = x'b -> phi
    ## We allow for beta to be a matrix of (p-by-lq)
    p <- dim(X)[2]
    beta <- matrix(beta, nrow = p)

    if(any(is.na(beta)))
    {
        stop("NA/NaN are not allowed for input `beta`.")
    }

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

    ## Cutoff for very big/small values that cause numerical instable.
    out.new <- parRestricFun(par = out, linkArgs)

    ## if(any(is.na(out))) browser()
    return(out.new)
}


## Slightly modify the par to avoid under/over flow in parLinkFun()
##' @export
parRestricFun <- function(par, linkArgs)
{
    tol <- 1e-6

    ## Extract the lower and upper bounds
    a <- linkArgs$a
    b <- linkArgs$b

    if(is.null(a)) a <- -Inf
    if(is.null(b)) b <- Inf

    ## No restrictions,  do nothing
    out <- par

    aM <- (par <=  a)
    if (is.finite(a) && any(aM))
    {
        out[aM] <- a + (a - par[aM])

        a0 <- (out == a)
        if(any(a0))
        {
            out[a0] <- a + tol
        }

        warning(sum(aM) ,
                " generated data points are outside parameter lower boundary. Corrected.")
    }

    bP <- (par >= b)
    if (is.finite(b) && any(bP))
    {
        out[bP] <- b - (par[bP]-b)

        b0 <- (out  == b)
        if(any(b0)) # very special case
        {
            out[b0] <- b - tol
        }

        warning(sum(bP) ,
                " generated data points are outside parameter upper boundary. Corrected.")
    }
    return(out)
}
