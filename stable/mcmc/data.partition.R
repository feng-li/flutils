##' Description
##'
##' Details.
##' @name 
##' @title 
##' @param n.obs 
##' @param args list N: no. of subsets,  method: how to partition
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Mon Sep 20 21:08:01 CEST 2010;
##'       Current:       Mon Sep 20 21:08:11 CEST 2010.
data.partition <- function(n.obs, args) 
{

  if(n.obs < args$N.subsets ||
     (args$N.subsets < 2 & tolower(args$partiMethod) != "time-series")) 
    {
      # Observation smaller than subsets. 
      stop("No. of subsets should be equal or smaller than no. of obs and greater than 1.")
    }
  
  obs.label <- 1:n.obs
  # disable warnings when ata length is not a multiple of split
                                        # variable which is what I want.
  suppressWarnings(
    length.out <- split(obs.label, 1:args$N.subsets)) # split the knots in a smart way. e.g. split
                                        # 20 knots into 3 folds
  
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
      testLen <- round(n.obs*args$testRatio)
      out <- list((n.obs-testLen+1):n.obs)
      ## for(i in 1:args$N.subsets)
      ##   {
      ##     out[[i]] <- start:(start+length(length.out[[i]])-1)
      ##     start <- out[[i]][length(out[[i]])] +1
      ##   }
    }

  else
    {
      stop("The partitioning method is not implemented!")
    }    
  names(out) <- NULL # remove the name. I don't like it.
  return(out)
}
