##' @export
dmixture <- function(x, type, par.list, log = FALSE)
{
    if(tolower(type) == "splitt")
    {
        mixture.mu = par.list[["mu"]]
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
