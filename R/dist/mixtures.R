##' @export
dmixture <- function(x, type, par.list, log = FALSE)
{
    if(tolower(type) == "splitt")
    {
        mixture.mu = par.list[["mu"]] # n-by-nComp matrix
        mixture.df = par.list[["df"]]
        mixture.phi = par.list[["phi"]]
        mixture.lmd = par.list[["lmd"]]
        mixture.weights = par.list[["weights"]]

        nComp <- ncol(mixture.weights)

        mixture.dens <- apply(matrix(1:nComp), 1, function(comp)
            dsplitt(x = x,
                    mu = mixture.mu[, comp],
                    df = mixture.df[, comp],
                    phi = mixture.phi[, comp],
                    lmd = mixture.lmd[, comp],
                    log = FALSE))

        out.raw <- x
        out.raw[] <- rowSums(mixture.dens*mixture.weights)
    }


    if(log == TRUE)
    {
        out <- log(out.raw)
    }
    else
    {
        out <- out.raw
    }
    return(out)
}

pmixture <- function(q, type, par.list,  log = FALSE)
{
    if(tolower(type) == "splitt")
    {
        mixture.mu = par.list[["mu"]]
        mixture.df = par.list[["df"]]
        mixture.phi = par.list[["phi"]]
        mixture.lmd = par.list[["lmd"]]
        mixture.weights = par.list[["weights"]]

        nComp <- ncol(mixture.weights)

        mixture.pct <- apply(matrix(1:nComp), 1, function(comp)
            psplitt(q = q,
                    mu = mixture.mu[, comp],
                    df = mixture.df[, comp],
                    phi = mixture.phi[, comp],
                    lmd = mixture.lmd[, comp]))

        out.raw <- q
        out.raw[] <- rowSums(mixture.pct*mixture.weights)
    }


    if(log == TRUE)
    {
        out <- log(out.raw)
    }
    else
    {
        out <- out.raw
    }
    return(out)
}

rmixture <- function(n, type, par.list)
{
    if(tolower(type) == "splitt")
    {
        mixture.mu = par.list[["mu"]] # n-by-nComp matrix
        mixture.df = par.list[["df"]]
        mixture.phi = par.list[["phi"]]
        mixture.lmd = par.list[["lmd"]]
        mixture.weights = par.list[["weights"]] # n-by-nComp matrix

        nComp <- ncol(mixture.weights)

        if(any(rapply(par.list, nrow) > 1))
        {
            stop("Only single row of parameter is allowed.")
        }

        idx <- rmultinom(n = n, 1, prob = mixture.weights) # nComp-by-nSim matrix

        out <- apply(idx, 2, function(x)
        {
            which.comp <- which(x == 1)
            rsplitt(n = 1,
                    mu = mixture.mu[, which.comp],
                    df = mixture.df[, which.comp],
                    phi = mixture.phi[, which.comp],
                    lmd = mixture.lmd[, which.comp])
        })
    }

    return(out)
}



qmixture <- function(p, type, par.list, args = list(nSim = 1000))
{
    if(tolower(type) == "splitt")
    {
        mixture.mu = par.list[["mu"]] # n-by-nComp matrix
        mixture.df = par.list[["df"]]
        mixture.phi = par.list[["phi"]]
        mixture.lmd = par.list[["lmd"]]
        mixture.weights = par.list[["weights"]] # n-by-nComp matrix

        nComp <- ncol(mixture.weights)
        nSim = args[["nSim"]]

        ## Reserver space
        mixture.rvs = matrix(NA, nSim, length(p))
        out.q = p

        for(i in 1:length(p))
        {
            mixture.rvs[, i] = rmixture(
                n = nSim, type = type,
                par.list = lapply(par.list, function(x) x[i,, drop = FALSE]))
            out.q[i] = quantile(mixture.rvs[, i], p[i])
        }

    }

    return(out.q)
}

mixture.mean <- function(type, par.list)
{
    if(tolower(type) == "splitt")
    {

        mixture.mu = par.list[["mu"]]
        mixture.df = par.list[["df"]]
        mixture.phi = par.list[["phi"]]
        mixture.lmd = par.list[["lmd"]]
        mixture.weights = par.list[["weights"]]

        nComp <- ncol(mixture.weights)

        mixture.mean <- apply(matrix(1:nComp), 1, function(comp)
            splitt.mean(mu = mixture.mu[, comp],
                        df = mixture.df[, comp],
                        phi = mixture.phi[, comp],
                        lmd = mixture.lmd[, comp]))

        out <- rowSums(mixture.mean*mixture.weights)
    }
    return(out)
}

mixture.var <- function(type, par.list)
{
    ## Sylvia Frühwirth-Schnatter, Finite Mixture and Markov Switching Models
    if(tolower(type) == "splitt")
    {

        mixture.mu = par.list[["mu"]]
        mixture.df = par.list[["df"]]
        mixture.phi = par.list[["phi"]]
        mixture.lmd = par.list[["lmd"]]
        mixture.weights = par.list[["weights"]]

        nComp <- ncol(mixture.weights)

        mixture.mean <- apply(matrix(1:nComp), 1, function(comp)
            splitt.mean(mu = mixture.mu[, comp],
                        df = mixture.df[, comp],
                        phi = mixture.phi[, comp],
                        lmd = mixture.lmd[, comp]))

        mixture.vars <- apply(matrix(1:nComp), 1, function(comp)
            splitt.var(mu = mixture.mu[, comp],
                       df = mixture.df[, comp],
                       phi = mixture.phi[, comp],
                       lmd = mixture.lmd[, comp]))

        out <- (rowSums((mixture.mean^2+mixture.vars)*mixture.weights) -
                rowSums(mixture.mean*mixture.weights)^2)
    }
    return(out)
}


## TESTING

## type = "splitt"
## nComp = 2
## n = 100
## p = runif(n)
## weights0 = matrix(runif(n * nComp), n, nComp)
## weights = weights0 / rowSums(weights0)
## par.list = list(
##     mu = matrix(runif(n * nComp), n, nComp),
##     df = matrix(rpois(n * nComp, lambda = 5) + 1, n, nComp),
##     phi = matrix(runif(n * nComp, 0.5, 1.5), n, nComp),
##     lmd = matrix(runif(n * nComp, 0.5, 1.5), n, nComp),
##     weights = weights)

## nSim = 1000
## ## mx.rvs = rmixture(n = nSim, type = type, par.list = par.list)
## out = qmixture(p, type = type, par.list = par.list, args = list(nSim = 1000))
## p1 = pmixture(out, type, par.list)
## var(p1 - p)
