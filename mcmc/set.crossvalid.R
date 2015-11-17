##' Setup cross-validation
##'
##'
##' @param n.obs
##' @param crossvalidArgs
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First: Thu May 03 14:36:26 CEST 2012; Current: Thu Aug 27 19:19:43 CST 2015
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
    if(tolower(partiMethod) == "time-series")
    {
      traning.subfun <- function(Testing.Idx)
      {
        1:(Testing.Idx[1]-1)
      }
    }
    else
    {
      traning.subfun <- function(Testing.Idx)
      {
        (1:nObs)[-Testing.Idx]
      }
    }

    ## The training subsets
    Data.training.sub <- lapply(Data.testing.sub, traning.subfun)
    out <- list(training = Data.training.sub,
                testing = Data.testing.sub)
  }
  return(out)
}
