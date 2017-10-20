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
  partiMethod <- args[["partiMethod"]]
  N.subsets <- args[["N.subsets"]]
  testRatio <- args[["testRatio"]]

  if(length(testRatio) == 0)
  {
      testRatio <- 0
  }

  if(nObs < N.subsets ||
     (N.subsets < 1 & tolower(partiMethod) != "time-series"))
  {
                                        # Observation smaller than subsets.
    stop("No. of subsets should be equal or smaller than no. of obs and at least one.")
  }

  ## disable warnings when ata length is not a multiple of split variable which is what I
  ## want.  split the knots in a smart way. e.g. split 20 knots into 3 folds
  obs.label <- 1:nObs
  suppressWarnings(length.out <- split(obs.label, 1:(N.subsets)))

  if(tolower(partiMethod) == "systematic")
  {## Select testing samples in a sequential order
    ## if(N.subsets == 1)
    ## {
    ##     out <- list()
    ##     out[["1"]] <- floor(seq(1,  nObs, length.out = ceiling(nObs*testRatio)))
    ## }
    ## else
    ## {
      out <- length.out
    ## }
  }
  else if(tolower(partiMethod) == "random")
  {## Randomly select testing samples
    out <- list()

    if(N.subsets == 1)
    {
      out[["1"]] <- sample(obs.label, ceiling(nObs*testRatio))
    }
    else
    {
      obs.label.new <- obs.label
      for(i in 1:N.subsets)
      {
        out[[i]] <- sample(obs.label.new, length(length.out[[i]]))
        obs.label.new <-  obs.label.new[!obs.label.new%in%out[[i]]]
      }
    }
  }
  else if(tolower(partiMethod) == "ordered")
  {
    out <- list()

    if(N.subsets == 1)
    {
      testLen <- ceiling(nObs*testRatio)
      out[["1"]] <- (nObs - testLen + 1):nObs
    }
    else
    {
      start <- 1
      for(i in 1:N.subsets)
      {
        out[[i]] <- start:(start+length(length.out[[i]])-1)
        start <- out[[i]][length(out[[i]])] +1
      }
    }
  }
  else if(tolower(partiMethod) == "time-series")
  {
    ## Use old data as testing data and recent data to preform predictions, the argument
    ## "testRatio" is used.

    if(is.null(testRatio))
    {
      stop("testRatio needed!")
    }

    if(N.subsets  != 1)
    {
      stop("Currently for time series,  the last ", testRatio*100,
           "% observations are used as a single test sample. No cross-validation applied.")

    }

    testLen <- ceiling(nObs*testRatio) # Make sure at least one is available
    start <- (nObs-testLen+1)

    ##obs.label.ts <- start:nObs
    ## suppressWarnings(length.out.ts <- split(obs.label.ts, 1:N.subsets))

    out <- list()
    ## for(i in 1:N.subsets)
    ## {
    out[["1"]] <- start:nObs
    ## start <- out[[i]][length(out[[i]])] +1
    ##  }
  }
  else
  {
    stop("The partitioning method is not implemented!")
  }
  names(out) <- NULL # remove the name. I don't like it.
  return(out)
}
