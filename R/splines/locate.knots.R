##' Locate knots in a m-space using Villani(2009)'s algorithm.
##'
##' This is the algorithm for determining the knots locations for a give global radius e>0
##' and a local radius skrinkage factor. The details can be foudn in Villani (2009)
##' @param x "matrix".
##'         The data matrix without intercept.
##' @param e "numeric".
##'         The radius for the Mahalanobis e-ball.
##' @param RadiusShrink "numeric".
##'         The radius shrinkage factor for the Mahalanobis e-ball
##'
##' @return "list" `n.knots` ("numeric") gives the no. of knots returned. `location`
##'     ("matrix") gives the knots location.
##'
##' @references  Appendix C. in Villani et al (2009)
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version:  Wed Mar 10 14:03:31  CET 2010;
##'       Current:        Sat Sep 18 18:57:24 CEST 2010.
##' @export
locate.knots <- function(x, e, RadiusShrink)
{
  dim.x <- dim(x)
  n <- dim.x[1] # no. of obs.
  m <- dim.x[2] # no. of covariates
  S <- cov(x)*(n-1) # m-by-m

  ## setup a tank for saving the knots.
  location.tmp <- matrix(0, n, m)

  ## initial values
  j <- 0 # no. of knots
  x.new <-x  # the remaining points
  while(length(x.new) > 0) # loop if x is not empty
    {
      j <- j + 1
      dist <- mahalanobis(x.new, colMeans(x.new), S)
      n.c <- sum(dist<= e) # no. of obs. in x.new belongs to e-ball
      e.new <- e/(n.c^RadiusShrink) # adapt the radius locally
      idx <- dist <= e.new # index of x.new in the ball,  TRUE or FALSE
      n.c.new <- sum(idx) # no. of obs. belongs to new e-ball
      x.new2 <- x.new[as.vector(idx), , drop = FALSE] # the obs. belongs to new e-ball
      if(n.c.new == 0 || n.c  == 0) # no knots have been selected in this ball due to a
                                    # bad radius shrinkage,  jump out the loop
        {
          j <- n # set no. of knots to an auxiliary number. that is sufficient.
          break # jump out
        }
      dist2 <-  mahalanobis(x.new2, colMeans(x.new2), S) # distance from points in
                                        # new e-ball to their mean.
      location.tmp[j, ] <- x.new2[which.min(dist2), , drop = FALSE]
      x.new <- x.new[!as.vector(idx), , drop = FALSE] # remove knots. update the data matrix.

    }
  location <- location.tmp[1:j, , drop = FALSE]
  out <- list(n.knots = j, location = location)
  return(out)
}

## This function is used for the root finding algorithm.
## See also make.knots() function
optim.knots.no <- function(e, xx, RadiusShrink, KnotsNo)
{
  no.knots <- locate.knots(xx, e, RadiusShrink)$n.knots
  gap <- no.knots - KnotsNo
  return(gap)
}
