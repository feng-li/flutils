##' Setup cross-validation 
##'
##' Details.
##' @title 
##' @param n.obs 
##' @param crossvalidArgs 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
set.crossvalid <- function(n.obs, crossValidArgs)
{
  ## No cross-validation
  if(crossValidArgs$N.subsets == 0) 
    {
      ## If no cross-validation, use full sample date. Just predictive all the
      ## insample data if required
      Data.training.sub <- list(1:n.obs)
      Data.testing.sub <- list(1:n.obs)
      out <- list(training = Data.training.sub,
                  testing = Data.testing.sub)
    }
  else
    {
      ## The predictive subsets. 
      Data.testing.sub <- data.partition(n.obs = n.obs, args = crossValidArgs)
      Data.training.sub <- lapply(Data.testing.sub, function(x) (1:n.obs)[-x])
      out <- list(training = Data.training.sub,
                  testing = Data.testing.sub)
    }

  ## Do cross-validation also include a full run. NOTE: Not very useful
  ## if(crossvalidArgs$full.run == TRUE && crossValidArgs$N.subsets != 1)
  ##   {
  ##     Data.training.sub[[crossValidArgs$N.subsets+1]] <-  1:n.obs
  ##     Data.testing.sub[[crossValidArgs$N.subsets+1]] <-  1:n.obs
  ##   }
  return(out)
}
