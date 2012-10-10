##' Setup cross-validation
##'
##'
##' @title
##' @param n.obs
##' @param crossvalidArgs
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Thu May 03 14:36:26 CEST 2012;
##'       Current: Thu May 03 14:36:39 CEST 2012.
set.crossvalid <- function(nObs, crossValidArgs)
{
  ## No cross-validation
  if(crossValidArgs$N.subsets == 0)
    {
      ## If no cross-validation, use full sample date. Just predictive all the
      ## insample data if required
      Data.training.sub <- list(1:nObs)
      Data.testing.sub <- list(1:nObs)
      out <- list(training = Data.training.sub,
                  testing = Data.testing.sub)
    }
  else
    {
      ## The predictive subsets.
      Data.testing.sub <- data.partition(nObs = nObs, args = crossValidArgs)
      Data.training.sub <- lapply(Data.testing.sub, function(x) (1:nObs)[-x])
      out <- list(training = Data.training.sub,
                  testing = Data.testing.sub)
    }

  ## Do cross-validation also include a full run. NOTE: Not very useful
  ## if(crossvalidArgs$full.run == TRUE && crossValidArgs$N.subsets != 1)
  ##   {
  ##     Data.training.sub[[crossValidArgs$N.subsets+1]] <-  1:nObs
  ##     Data.testing.sub[[crossValidArgs$N.subsets+1]] <-  1:nObs
  ##   }
  return(out)
}
