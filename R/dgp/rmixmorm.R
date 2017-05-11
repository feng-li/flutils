##' Generate random variables from mixture normal distribution
##'
##' Random variables from mixture of normals.
##' @title test title
##' @param n "integer", numbers of samples to be generated
##' @param means "q-by-k matrix" mean value within each component, total k components.
##' @param sigmas "q-by-q-by-k" variance covariance matrix with in each component
##' @param weights "k-length vector" weights in each component
##' @return "matrix"
##' @references Villani et al 2009
##' @author Feng Li, Central University of Finance and Economics.
rmixnorm <- function(n, means, sigmas, weights)
{

    if(!is.matrix(means) | length(dim(sigmas)) != 3)
    {
        stop("means must be a q-by-k matrix and sigmas must be a q-by-q-by-k array.")
    }

    k <- length(weights) # K-components
    q <- dim(means)[1] # q-dimensional

    idx <- rmultinom(n = n, 1, prob = weights) # k-by-n matrix

    out <- apply(idx, 2, function(x, means, sigmas, q)
    {
        which.comp <- which(x == 1)
        rmvnorm(n = 1, mean = means[, which.comp],
                sigma = matrix(sigmas[, , which.comp]))
    }, means = means, sigmas = sigmas, q = q)

    return(out)
}

## Tests
## n <- 1000
## means <- matrix(c(-5, 0, 5), 1)
## sigmas <- array(c(1, 1, 1), c(1, 1, 3))
## weights <- c(0.3, 0.4, 0.3)
## out <- rmixnorm(n, means, sigmas, weights)
## hist(out, breaks = 100, freq = FALSE)


##' Simulate AR type random variables from mixture of normal
##'
##' This function simulates random samples from a finite mixture of Gaussian distribution
##'     where the mean from each components are AR(p) process.
##' @param n number of samples
##' @param means.ar.par.list parameters in AR(p) within each mixing compoment
##' @param sigmas.list variance list
##' @param weights weight in each list
##' @param yinit  initial values
##' @return vector of n follows a mixture distribution
##' @references Li 2010 JSPI
##' @author Feng Li, Central University of Finance and Economics.
rmixnorm.ts <- function(n, means.ar.par.list, sigmas.list, weights, yinit = 0)
{
    y <- rep(NA, n)
    nComp <- length(means.ar.par.list)
    nLags <- lapply(means.ar.par.list, function(x) length(x)-1)
    maxLag <- max(unlist(nLags))

    if(any(unlist(nLags)<1))
    {
        stop("Drift is always included. Set the first elements in ar.par.list be zero to remove the drift effect.")
    }

    y[1:maxLag] <- yinit

    sigmas.ary <- matrix(do.call(cbind, sigmas.list), n, nComp, byrow = TRUE)

    for(i in (maxLag+1):n)
    {
        meansComp <- lapply(means.ar.par.list, function(par, y, i){
            nLag <- length(par)-1
            yPre <- c(1, y[(i-1):(i-nLag)])
            yCurr <- sum(par*yPre)
            ## print(cbind(yPre, par))
            return(yCurr)
        }, y = y, i = i)

        sigmas.i <- array(sigmas.ary[i, ], dim = c(1, 1, nComp))
        y[i] <- rmixnorm(n = 1, means = matrix(unlist(meansComp), nrow = 1),
                         sigmas = sigmas.i, weights = weights)
    }
    return(y)
}


## Tests
## n = 1000
## means.ar.par.list = list(c(0, 0.8), c(0, 0.6, 0.3))

## require("fGarch")
## sigmas.spec <- list(garchSpec(model = list(alpha = c(0.05, 0.06)), cond.dist = "norm"),
##                     garchSpec(model = list(alpha = c(0.05, 0.05)), cond.dist = "norm"))
## sigmas.list <- lapply(lapply(sigmas.spec, garchSim, extended = TRUE, n = n),
##                       function(x) x$sigma)
## weights <- c(0.8, 0.2)
## y = rmixnorm.ts(n = n, means.ar.par.list = means.ar.par.list, sigmas.list = sigmas.list,
##                 weights = weights)
