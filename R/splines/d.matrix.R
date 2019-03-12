##' Make a design matrix for multivariate thin plate splines and others.
##'
##' The number of knots should be smaller than the number of observations.
##'
##' @name d.matrix
##' @title Design matrix for splines
##'
##' @param x "matrix".
##'         the data to be used in the splines. You should *NOT* include a
##'         constant column. The intercept will be added automatically if required in
##'         "args".
##' @param knots "list".
##'         knots locations for different spline components:
##'         knots$thinplate.a: "matrix" Knots for the additive part.
##'         knots$thinplate.s: "matrix" Knots for the surface part.
##' @param splineArgs "list".
##'         Arguments list pass to the function, see bellow.
##'         splineArgs$comp: "character". The combination of splines, which can be
##'         "intercept": If included, will return design matrix with intercept.
##'         "covariates": If included, covariates should be included
##'         "thinplate.s": If included, the thinplate surface will be in;
##'         "thinplate.a": If include, the additive thinplate will be in.
##'         e.g. c("intercept", "covariates", "thinplate.s", "thinplate.a") means all the
##'         four components will be in the design matrix.
##'         splineArgs$thinplate.a.locate: "numeric", c(2, 3, 0) means 2 knots
##'         for first regressor, 3 knots for the second regressor and no knots for third
##'         regressor.
##'
##' @return "matrix".
##'         The dimension depends on the arguments, and the first column is 1 as an intercept
##'         if required in "splineArgs".
##'
##' @references Denison et al (2002)
##' @author Feng Li, Dept. of Statistics, Stockholm University, Sweden.
##' @note First version: Sun Mar  7 13:41:32  CET 2010.
##'       Current:       Tue Dec 28 13:34:39 CET 2010.
##'
##' @export
d.matrix <- function(x,knots,splineArgs)
{

  ## Pre check
  dim.x <- dim(x)
  n <- dim.x[1] # no. of obs.
  m <- dim.x[2] # no. of covs.
  comp <- tolower(splineArgs$comp) # The components of the design matrix
  out.tmp <- list()

  ## Construct the intercept.
  if ("intercept" %in% comp)
    {
      out.tmp[["intercept"]] <- matrix(1, dim.x[1], 1)
    }

  ## Construct the covariates.
  if ("covariates" %in% comp)
    {
      out.tmp[["covariates"]] <- x
    }

  ## Construct the thinplate sufrace
  if ("thinplate.s" %in% comp)
    {
      log.rowDist <- rdist(x, knots[["thinplate.s"]], log = TRUE)
      thinplate.s <- exp(2*log.rowDist)*log.rowDist # P. 141, Denison et al. (2002)

      ## when x -> 0,  x^2log(x)->0 It may produce NaN numerically at this situation.
      thinplate.s[is.nan(thinplate.s)] <- 0
      out.tmp[["thinplate.s"]] <- thinplate.s
    }

  ## Construct additive thinplate spline
  if ("thinplate.a" %in% comp)
    {
      idx <- rep(1:m, splineArgs$thinplate.a.locate)

      x.add <- x[, idx, drop = FALSE]
      knots.add <- matrix(knots[["thinplate.a"]], n, length(idx), byrow = TRUE)

      dist.add <- x.add-knots.add
      thinplate.a <- (dist.add)^2*log(abs(dist.add))

      ## Numerical corrections. x^2*log(x) -> 0 when x -> 0.
      ## But it was NaN numerically.
      thinplate.a[is.nan(thinplate.a)] <- 0

      out.tmp[["thinplate.a"]] <- thinplate.a
    }

  ## Order and combine the output matrix.
  X.out <- matrix(unlist(out.tmp[comp]), n)

  return(X.out)
}
##----------------------------------------------------------------------------------------
## TESTS: PASSED
##----------------------------------------------------------------------------------------
## x <- matrix(rnorm(20), 5, 4)
## knots <- list(thinplate.s = matrix(rnorm(8), 2, 4),
##               thinplate.a = rnorm(5))
## splineArgs <- list(comp = c("intercept", "covariates", "thinplate.s", "thinplate.a"),
##                    thinplate.a.locate = c(2, 0, 1, 2))
## d.matrix(x, knots, splineArgs)
