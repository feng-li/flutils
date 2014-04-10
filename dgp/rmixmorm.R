##' Generate random variables from mixture normal distribution
##'
##'
##' @title Random variables from mixture of normals
##' @param n "integer",  numbers of samples to be generated
##' @param means "q-by-k matrix" mean value within each component
##' @param sigmas "q-by-q-by-k" variance covariance matrix with in each component
##' @param weights "k-length vector" weights in each component
##' @return "matrix"
##' @references Villani et al 2009
##' @author Feng Li, Central University of Finance and Economics.
##' DEPENDS: rmvnorm
rmixnorm <- function(n, means, sigmas, weights)
    {

        if(!is.matrix(means) | length(dim(sigmas)) != 3)
            {
                stop("means must be a q-by-k matrix and sigmas must be a q-by-q-by-k array.")
            }

        k <- length(weights) # K-components
        q <- dim(means)[1] # q dimensional

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
