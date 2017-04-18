csolve <- function(FUN,x,...,gradfun=NULL,crit=1e-7,itmax=20,verbose=TRUE,alpha=1e-3,delta=1e-6,long=FALSE) {
### FUN:      A function with vector argument x or (when numerical derivatives are used) with matrix argument x.
###           the number of rows in x matches the number of rows in the return value.  For numerical derivatives,
###           the number of columns in x is the number of distinct argument vectors at which FUN is evaluated.
###           **************************************
### x:        initial value for FUN's argument.
### gradfun:  the function called to evaluate the gradient matrix.  If this
###           is NULL (the default), a numerical gradient is used instead.  If it is identical to
###              FUN, then FUN returns a value v with attr(v,"grad") the gradient matrix.
### crit:     if the sum of absolute values that FUN returns is less than this,
###           the equation is solved.
### itmax:    the solver stops when this number of iterations is reached, with rc=4
### ...:      in this position the user can place any number of additional arguments, all
###           of which are passed on to FUN and gradfun (when it is non-empty) as a list of 
###           arguments following x.
### -------------------------------------------------------------------
###           Arguments below this usually can be left at defaults.
### verbose:  If set to FALSE, the amount of output during iterations is cut to zero.
### long:     Set to TRUE to suppress printout of x and f at each iteration. (No effect if verbose=FALSE)
###           Useful when x and FUN are long vectors.
### alpha:    Tolerance on rate of descent.  The algorithm may produce search directions nearly
###           orthogonal to the gradient, and hence nearly zero directional derivative.  Smaller
###           alpha allows closer approach to orthogonality.
### delta:    difference interval in (cheap, forward-difference) numerical derivative.  Ignored if gradfun non-NULL.
### rc:       0 means normal solution, 1 and 3 mean no solution despite extremely fine adjustments
###           in step length (very likely a numerical problem, or a discontinuity). 4 means itmax
###           termination.
### -------------------------------------------------------------------
### modified 12/23/05 to allow everything to be complex-valued.  Modifications only lightly tested.
###------------ analyticg --------------
  analyticg <- !is.null(gradfun) #if the grad argument is NULL, numerical derivatives are used.
  jointg <- identical(FUN, gradfun) # FUN returns both value and gradient
###-------------------------------------
  EPS <- .Machine$double.eps
  if (is.null(dim(x))) {
    vdn <- names(x)
    x <- matrix(x,length(x),1)
    dimnames(x)[[1]] <- vdn
  }
  nv <- dim(x)[1]
  vdn <- dimnames(x)
  tvec <- delta*diag(nv)
  done <- FALSE
  f0 <- FUN(x,...)
  af0 <- sum(abs(f0))
  af00 <- af0
  itct <- 0
  while(!done) {
    if((itct%%2)==1  && af00-af0 < crit*max(1,af0) && itct>3) {
      randomize <- TRUE
    } else {
      if(!analyticg) {
        grad <- (FUN(matrix(x,nv,nv,dimnames=vdn)+tvec,...)-matrix(f0,nv,nv))/delta
      } else {                          # use analytic gradient
        if (jointg) {
          grad <- attr(f0, "grad")
        } else {
          grad <- gradfun(x,...)
        }
      }
      ## if(is.real(grad) && is.finite(grad) && sum(abs(grad))> 4*nv^2*EPS) {
      if(is.finite(grad) && sum(abs(grad))> 4*nv^2*EPS) {
        svdg <- svd(grad)
        svdd <- svdg$d
        if(!(min(svdd)>0) || max(svdd)/min(svdd) > 1/(100*EPS)){
          svdd <- pmax(svdd,max(svdd)*1e-13)
          grad <- svdg$u%*% diag(svdd,ncol=length(svdd)) %*% t(svdg$v)
        }
        dx0 <- -solve(grad,f0)
        randomize <- FALSE
      } else {
        if(verbose){cat("gradient imaginary or infinite or zero\n")}
        randomize <- TRUE
      }
    }
    if(randomize) {
      if(verbose){cat("Random Search\n")}
      dx0 <- sqrt(sum(x*x))/matrix(rnorm(length(x)),length(x),1)
    }
    lambda <- 1
    lambdamin <- 1
    fmin <- f0
    xmin <- x
    afmin <- af0
    dxSize <- sqrt(sum(abs(dx0)^2))
    factor <- .6
    shrink <- TRUE
    subDone <- FALSE
    while(!subDone) {
      dx <- lambda*dx0
      f <- FUN(x+dx,...)
      af <- sum(abs(f))
      if(!is.nan(af) && af < afmin) {
        afmin <- af
        fmin <- f
        lambdamin <- lambda
        xmin <- x+dx
      }
      if (((lambda >0) && (is.nan(af) || (af0-af < alpha*lambda*af0))) || ((lambda<0) && (is.nan(af) || (af0-af < 0)) )) {
        if(!shrink) {
          factor <- factor^.6
          shrink <- TRUE
        }
        if(abs(lambda*(1-factor))*dxSize > .1*delta) {
          lambda <- factor*lambda
        } else {
          if( (lambda > 0) && (factor==.6) ) { #i.e., we've only been shrinking
            lambda <- -.3
          } else {
            subDone <- TRUE
            if(lambda > 0) {
              if(factor==.6) {
                rc <- 2
              } else {
                rc <- 1
              }
            } else {
              rc <- 3
            }
          }
        }
      } else {
        ## if( (lambda >0) && (af-af0 > (1-alpha)*lambda*af0) ) {
        ## I think af-af0 instead of af0-af in the line above is a long-standing error that
        ## meant this branch was never visited.  (cas 7/16/2009)
        if( (lambda >0) && (af0 - af > (1-alpha)*lambda*af0) ) {
          if(shrink) {
            factor <- factor^.6
            shrink <- FALSE
          }
          lambda <- lambda/factor
        } else {                        # good value found
          subDone <- TRUE
          rc <- 0
        }
      }
    }                                   # while ~subDone
    itct <- itct+1
    if(verbose){
      cat(paste("itct ",itct,", af ",afmin,", lambda ", lambdamin,", rc ",rc),"\n")
      if ( !long ) {
        if ( !is.complex(xmin) && !is.complex(fmin) ) {
          cat("x: \n",formatC(xmin,width=10,digits=6),"\n")
          cat("fmin:\n",formatC(fmin,width=10,digits=6),"\n")
        } else {
          cat("x: \n")
          print(as.complex(x))
          cat("fmin:\n")
          print(as.complex(fmin))
        }
      }
    }
    x <- xmin
    f0 <- fmin
    af00 <- af0
    af0 <- afmin
    if(itct >= itmax) {
      done <- TRUE
      rc <- 4
    } else {
      if(af0<crit) {
        done <- TRUE
        rc <- 0
      }
    }
  }                                     #while not done
  return(list(x=xmin,f=fmin,itcount=itct,retcode=rc))
}
