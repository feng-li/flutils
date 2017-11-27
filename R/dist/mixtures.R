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

    }

    return(out)
}
