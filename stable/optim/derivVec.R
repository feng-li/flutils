derivVec <- function(ex, x, param) {
  ##    ex:    a vector of expressions
  ##     x:    a character vector of variable names w.r.t. which the expressions will be differentiated
  ## param:    a vector of parameter values that will be constant in repeated calls to fret (the returned function)
  ##-------------------
  ##  fret:    returned value; a function that when evaluated at a numerical x, 
  ##           returns the vector of expression values, but also, as attr(value, "grad"), the gradient matrix
  ##           It uses the fixed param values set in the call to derivVec.
  nq <- length(ex)
  nv <- length(x)
  param <- param                        #to put param into this namespace, so it does not need to be in every call.
  outf <- vector("expression",nq)
  for (iq in 1:nq) {
    f <- deriv(ex[iq], x)
    outf[iq] <- f
  }
  fret <- function(z) {
    fval <- vector("numeric", nq)
    gval <- matrix(0, nq, nv)
    zv <- as.vector(z)
    names(zv) <- if (is.null(names(z))) dimnames(z)[[1]] else names(z)
    for (iq in 1:nq) {
      fg <- eval(outf[iq], as.list(c(zv, param)))
      fval[iq] <- fg
      gval[iq, ] <- attr(fg, "grad")
    }
    fval <- c(fval)
    names(fval) <- names(ex)
    dimnames(gval) <- list(eq=names(ex), vbl=names(zv))
    attr(fval, "grad") <- gval
    return(fval)
  }
  return(fret)
}
