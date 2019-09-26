##' Setup cross-validation
##'
##' Tools to setup cross-validations
##' @param n.obs Number of observations.
##' @param crossvalidArgs Cross-validation arguments.
##' @return list
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @export
set.crossvalid <- function(nObs, crossValidArgs)
{
    N.subsets <- crossValidArgs[["N.subsets"]]

    ## No cross-validation
    if(N.subsets == 0)
    {
        ## If no cross-validation, use full sample date. Just predictive all the insample data
        ## if required.
        Data.training.sub <- list(1:nObs)
        Data.testing.sub <- list(1:nObs)
        out <- list(training = Data.training.sub, testing = Data.testing.sub)
    }
    else
    {
        ## The predictive subsets.
        Data.testing.sub <- data.partition(nObs = nObs, args = crossValidArgs)

        partiMethod <- crossValidArgs[["partiMethod"]]

        ## The training index function
        traning.subfun <- function(Testing.Idx)
        {
            (1:nObs)[-Testing.Idx]
        }

        ## The training subsets
        Data.training.sub <- lapply(Data.testing.sub, traning.subfun)
        out <- list(training = Data.training.sub,
                    testing = Data.testing.sub)
    }
    return(out)
}
