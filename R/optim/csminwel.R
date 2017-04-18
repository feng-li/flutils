csminwel0 <- function(fcn,x0,H0,...,grad=NULL,crit=1e-7,nit,Verbose=TRUE,Long=FALSE) {
### fcn:   the objective function to be minimized
### x0:    initial value of the parameter vector
### H0:    initial value for the inverse Hessian.  Must be positive definite.
### grad:  A function that calculates the gradient, or the null matrix, or a numerical
###        vector.  If it's not a function, numerical gradients are used.  If it's numerical, the
###        first iteration uses the numerical vector as the initial gradient.  This is useful when the algorithm
###        is restarted and the gradient calculation is slow, since the program stores the final gradient when it quits.
###        If it's a function, it must return a list, with elements named g and badg.  g is the gradient and badg
###        is logical, TRUE if the gradient routine ran into trouble so its return value should not be used.
### crit:  Convergence criterion.  Iteration will cease when it proves impossible to improve the
###        function value by more than crit.
### nit:   Maximum number of iterations.
### ...:   A list of optional length of additional parameters that get handed off to fcn each
###        time it is called.
###        Note that if the program ends abnormally, it is possible to retrieve the current x,
###        f, and H from the files g1.mat and H.mat that are written at each iteration and at each
###        hessian update, respectively.  (When the routine hits certain kinds of difficulty, it
###        write g2.mat and g3.mat as well.  If all were written at about the same time, any of them
###        may be a decent starting point.  One can also start from the one with best function value.)
  dots <- list(...)                     # (need this to save these arguments in case of cliffs)
  nx <- length(x0)
  NumGrad <- !is.function(grad)
  done <- 0
  itct <- 0
  fcount <- 0
  snit <- 100
  badg1 <- badg2 <- badg3 <- FALSE
  f0 <- fcn(x0,...)
  ## browser()
  if( f0 > 1e50 ) {
    stop("Bad initial parameter.")
  }
  if( NumGrad ) {
    if( !is.numeric(grad) ) {
      gbadg <- numgrad(fcn,x0,...)
      g <- gbadg$g
      badg <- gbadg$badg
    } else {
      badg <- any(grad==0)
      g <- grad
    }
  } else {
    gbadg <- grad(x0,...)
    badg <-gbadg$badg
    g <- gbadg$g
  }
  retcode3 <- 101
  x <- x0
  f <- f0
  H <- H0
  cliff <- 0
  while( !done ) {
    g1 <- NULL; g2 <- NULL; g3 <- NULL
    ##addition fj. 7/6/94 for control
    cat('-----------------\n')
    cat('-----------------\n')
    cat(sprintf('f at the beginning of new iteration, %20.10f',f),"\n")
    if (!Long && Verbose) {             # set Long=TRUE if parameter vector printouts too long
      cat("x =\n")
      print(x,digits=12)
    }
    ##-------------------------
    itct <- itct+1
    itout <- csminit(fcn,x,f,g,badg,H,...)
    f1 <- itout$fhat
    x1 <- itout$xhat
    fc <- itout$fcount
    retcode1 <- itout$retcode
    fcount <- fcount+fc
    if( retcode1 != 1 ) {               # Not gradient convergence
      ## if( retcode1==2 || retcode1==4) {
      ##   wall1 <- TRUE; badg1 <- TRUE
      ## } else {                          # not a back-forth wall, so check gradient
      if( NumGrad ) {
        gbadg <- numgrad(fcn, x1,...)
      } else {
        gbadg <- grad(x1,...)
      }
      g1 <- gbadg$g
      badg1 <- gbadg$badg
      wall1 <- (badg1 || retcode1==2 || retcode1 == 4) # A wall is back-forth line search close or bad gradient
      ## g1
      save(file="g1", g1, x1, f1, dots)
      ## }
      if( wall1 && dim(H)[1] > 1) {
        ## Bad gradient or back and forth on step length.  Possibly at
        ## cliff edge.  Try perturbing search direction, if problem is not unidimensional
        ##
        Hcliff <- H+diag(diag(H) * rnorm(nx))
        cat("Cliff.  Perturbing search direction. \n")
        itout <- csminit(fcn,x,f,g,badg,Hcliff,...)
        f2 <- itout$fhat
        x2 <- itout$xhat
        fc <- itout$fcount
        retcode2 <- itout$retcode
        fcount <- fcount+fc             # put by Jinill
        ## if(  f2 < f ) {
        ##   if( retcode2 == 2 || retcode2==4 ){
        ##     wall2 <- 1
        ##     badg2 <- 1
        ##   } else {
        if( NumGrad ){
          gbadg <- numgrad(fcn, x2,...)
        }else{
          gbadg <- grad(x2,...)
        }
        g2 <- gbadg$g
        badg2 <- gbadg$badg
        wall2 <- (badg2 || retcode2==2 || retcode2==4)
        ## g2
        ## badg2
        save(file="g2", g2, x2, f2, dots)
        if( wall2 ) {
          cat('Cliff again.  Try traversing\n')
          normdx <- sqrt(sum(x2-x1)^2)
          if( normdx < 1e-13 ) {        # two trial x's too close, can't traverse
            f3 <- f; x3 <- x; badg3 <- 1;retcode3 <- 101
          } else {
            ## as.numeric below for robustness against f's being 1x1 arrays
            gcliff <- (x2 - x1) * (as.numeric(f2 - f1)/(normdx^2))
            dim(gcliff) <- c(nx,1)
            itout <- csminit(fcn,x,f,gcliff,0,diag(nx),...)
            f3 <- itout$fhat
            x3 <- itout$xhat
            fc <- itout$fc
            retcode3 <- itout$retcode
            fcount <- fcount+fc         # put by Jinill
            ## if( retcode3==2 || retcode3==4 ) {
            ##   wall3 <- 1
            ##   badg3 <- 1
            ## } else {
            if( NumGrad ) {
              gbadg <- numgrad(fcn, x3,...)
            }else{
              gbadg <- grad(x3,...)
            }
            g3 <- gbadg$g
            badg3 <- gbadg$badg
            wall3 <- (badg3 || retcode3==2 || retcode3==4)
            ## g3
            ## badg3
            save(file="g3", g3, x3, f3, dots)
          } 
        } else {                        # wall1, but not wall2, so pack f3, etc with initial values
          f3 <- f
          x3 <- x
          badg3 <- 1
          retcode3 <- 101
        }
      } else {                          # no walls, or one-dimensional, so no perturbations
        f3 <- f
        x3 <- x
        badg3 <- TRUE
        retcode3 <- 101
        f2 <- f
        x2 <- f
        retcode2 <- 101
      }
    } else {                            # gradient convergence --- csminit just looked at gradient and quit
      f1 <-  f2 <-  f3 <- f; g1 <- g <- FALSE; badg2 <-  badg3 <- TRUE; retcode1 <- 1; retcode2 <- 101; retcode3 <- 101
    }
    ## how to pick gh and xh:
    ## pick first fj that improves by crit and has badg==FALSE, starting with j=3
    ## may pick other than min fj to stay away from cliff
    if( f3 < f-crit & badg3==0 ) {
      ih <- 3
      fh <- f3;xh <- x3;gh <- g3;badgh <- badg3;retcodeh <- retcode3
    } else {                              
      if( f2 < f-crit && badg2==0 ) {
        ih <- 2
        fh <- f2;xh <- x2;gh <- g2;badgh <- badg2;retcodeh <- retcode2
      } else {
        if( f1 < f-crit && badg1==0 ) {
          ih <- 1
          fh <- f1;xh <- x1;gh <- g1;badgh <- badg1;retcodeh <- retcode1
        } else {
          ## none qualify, so pick the one that improves most.
          fh <- min(f1,f2,f3)
          ih <- which.min(c(f1,f2,f3))
          cat("ih =",ih,"\n")
          xh <- switch(ih,x1,x2,x3)
          retcodeh <- switch(ih,retcode1,retcode2,retcode3)
          nogh <- (!exists("gh",inherits=FALSE) || is.null(gh))
          if( nogh ) {
            if( NumGrad ) {
              gbadg <- numgrad(fcn,xh,...)
            } else {
              gbadg <- grad( xh,...)
            }
            gh <- gbadg$gh
            badgh <- gbadg$badg
          }
          badgh <- 1
        }
      }
    }
    ## end of picking
    ##ih
    ##fh
    ##xh
    ##gh
    ##badgh
    stuck <- (abs(fh-f) < crit)
    if ( (!badg) && (!badgh) && (!stuck) ) {
      H <- bfgsi(H,gh-g,xh-x)
    }
    ## if( Verbose ) {
    cat("----\n")
    cat(sprintf('Improvement on iteration %8.0f = %18.9f\n',itct,f-fh))
    ##}
    if( itct > nit ) {
      cat("iteration count termination\n")
      done <- 1
    } else {
      if( stuck ) {
        cat("improvement < crit termination\n")
        done <- 1
      }
      rc <- retcodeh
      switch( rc ,
             cat("zero gradient\n"),    #1
             cat("back and forth on step length never finished\n"), #2
             cat("smallest step still improving too slow\n"), #3
             cat("back and forth on step length never finished\n"), #4
             cat("largest step still improving too fast\n"), #5
             cat("smallest step still improving too slow, reversed gradient\n"), #6
             cat("warning: possible inaccuracy in H matrix\n"), #7
             )
    }
    f <- fh
    x <- xh
    g <- gh
    badg <- badgh
  }                                     # while !done
  return(list(fh=fh,xh=xh,gh=gh,H=H,itct=itct,fcount=fcount,retcodeh=retcodeh,...))
}
