##' Partition data index for cross validation.
##'
##'
##' @param nObs "positive numeric"
##' @param args list N: no. of subsets,  method: how to partition
##' @return "list" The prediction subsets.
##' @references Li Villani Kohn 2010
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Mon Sep 20 21:08:01 CEST 2010;
##'       Current:       Mon Sep 20 21:08:11 CEST 2010.
data.partition <- function(nObs, args)
{

  if(nObs < args$N.subsets ||
     (args$N.subsets < 2 & tolower(args$partiMethod) != "time-series"))
    {
      # Observation smaller than subsets.
      stop("No. of subsets should be equal or smaller than no. of obs and greater than 1.")
    }

  obs.label <- 1:nObs
  ## disable warnings when ata length is not a multiple of split variable which
  ## is what I want.  split the knots in a smart way. e.g. split 20 knots into
  ## 3 folds

  suppressWarnings(
    length.out <- split(obs.label, 1:args$N.subsets))
  if(tolower(args$partiMethod) == "systematic")
    { out <- length.out }
  else if(tolower(args$partiMethod) == "random")
    {
      out <- list(NULL)
      obs.label.new <- obs.label
      for(i in 1:args$N.subsets)
        {
          out[[i]] <- sample(obs.label.new, length(length.out[[i]]))
          obs.label.new <-  obs.label.new[!obs.label.new%in%out[[i]]]
        }
    }
  else if(tolower(args$partiMethod) == "ordered")
    {
      out <- list(NULL)
      start <- 1
      for(i in 1:args$N.subsets)
        {
          out[[i]] <- start:(start+length(length.out[[i]])-1)
          start <- out[[i]][length(out[[i]])] +1
        }
    }
  else if(tolower(args$partiMethod) == "time-series")
    {
      ## Use old data as testing data and recent data to preform predictions,
      ## the argument "testRatio" is used.
      ## out <- vector("list", length = 2)
      testLen <- ceiling(nObs*args$testRatio) # Make sure at least one is available
      start <- (nObs-testLen+1)
      obs.label.ts <- start:nObs

      suppressWarnings(
          length.out.ts <- split(obs.label.ts, 1:args$N.subsets))

      ## out <- list((nObs-testLen+1):nObs)
      out <- list(NULL)
      for(i in 1:args$N.subsets)
          {
              out[[i]] <- start:(start+length(length.out.ts[[i]])-1)
              start <- out[[i]][length(out[[i]])] +1
          }
    }
  else
    {
      stop("The partitioning method is not implemented!")
    }
  names(out) <- NULL # remove the name. I don't like it.
  return(out)
}
