#' Setup knots for spline models.
#'
#' This function can be used in the initial values for splines
#' @name make.knots
#' @title Locate the knots in m-space.
#'
#' @param x "matrix".
#'         n-by-m. Covariates matrix *without* intercept.
#' @param n.knots "integer".
#'         Number of knots used.
#' @param method "character".
#'         Method to be use in the knots locating method. Currently value are "k-means",
#'         "mahalanobis-eball" which came from Villani et al (2009) and "es" for equal
#'         spaced sample quantile with single covariate and "random".
#' @param args "list".
#'         Other arguments need to pass to the function w.r.t different "method". When
#'         method is "mahalanobis-eball", you need to provide:
#'         args$RadiusShrink: "numeric", the radus shrinkage for the ball.
#'
#' @return "list".
#'         A list with knots locations for given numbers of knots.
#'
#' @references Appendix C. in Villani et al (2009)
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#'
#' @note First version: Wed Mar 10 14:03:31  CET 2010;
#'       Current:       Thu Sep 16 13:56:37 CEST 2010.
#' TODO:
#' @export
make.knots <- function(x, method, splineArgs)
{
  if(!is.matrix(x))
    {
      stop("x should be a matrix.")
    }

  out <- list()
  ks <- splineArgs$thinplate.s.dim[1]

  if(tolower(method) == "no-knots") # ad-hoc for regression without knots
    {
      ## if("thinplate.s"  %in% splineArgs$comp)
      ##   {
          out[["thinplate.s"]] <- matrix(NA, ks, dim(x)[2])
      ##   }
      ## if("thinplate.a"  %in% splineArgs$comp)
      ##   {
          out[["thinplate.a"]] <- matrix(NA, sum(splineArgs$thinplate.a.locate), 1)
        ## }

    }
  else if(tolower(method) == "k-means")
    {
      iter.max <- 200
      if("thinplate.s"  %in% splineArgs$comp)
        {
          if(ks == 1) # Only one cluster
            {
              location <- matrix(colMeans(x), 1)
            }
          else
            {
              location <- kmeans(x, centers = ks, iter.max = iter.max)$centers
              rownames(location) <- NULL # remove the rownames
            }
          out[["thinplate.s"]] <- location
        }
      if("thinplate.a" %in% splineArgs$comp)
        {
          a.locate <- splineArgs$thinplate.a.locate
          ## x.idx4knots <- rep(1:m, a.locate)

          x.noempty <- x[, a.locate != 0, drop = FALSE]
          x.noempty.list <- array2list(x.noempty, 2)
          nknots <- a.locate[a.locate != 0]
          location <- mapply(x.noempty.list, FUN = function(x, centers) kmeans(x, centers,
                                               iter.max = iter.max)$centers, centers = as.list(nknots))
          out[["thinplate.a"]] <- matrix(unlist(location))
        }

    }
  else if(tolower(method) == "random") # TODO: Maybe a good consideration if no. of knots
                                        # exceed no. of obs.
    {
      if("thinplate.s"  %in% splineArgs$comp)
        {
          dim.x <- dim(x)
          n <- dim.x[1] # no. of obs.
          idx <- sample(1:n, ks)
          location <- x[idx, ,drop = FALSE]
          out[["thinplate.s"]] <- location
        }
      if("thinplate.a"  %in% splineArgs$comp)
        {
          dim.x <- dim(x)
          n <- dim.x[1] # no. of obs.
          a.locate <- splineArgs$thinplate.a.locate

          nknots <- a.locate[a.locate!=0]
          x.noempty <- x[, a.locate!=0, drop = FALSE]
          x.noempty.list <- array2list(x.noempty, 2)
          location <-  mapply(x.noempty.list, FUN = function(x, nknots)  x[sample(1:n,
                                                nknots)], nknots = as.list(nknots))
          out[["thinplate.a"]] <- matrix(unlist(location))
        }
    }
  else if(tolower(method)=="equal-spaced")
    {
      if(dim(x)[2] != 1)
        {
          stop("equal-spaced only works with single covariates")
        }
      ## Equal spaced sample quantile for single covariate.  TODO: make this for
      ## multivariate covariates.
      n.knots <- "Not ready!"
      probs <- seq(0,1,length.out=n.knots+2)[-c(1,n.knots+2)]
      xi <- matrix(quantile(x,probs),length(x),1)
    }
  else
    {
      stop("Wrong input argument for method!")
    }
  return(out)
}


##----------------------------------------------------------------------------------------
## TESTS: PASSED
##----------------------------------------------------------------------------------------
## x <- matrix(1:100, 20)
## method <- "k-means"
## splineArgs <- list(comp = c("thinplate.a", "thinplate.s"),
##                    thinplate.s.N = 3,
##                    thinplate.a.locate = c(0, 1, 2, 0))
## a <- make.knots(x, method, splineArgs)
