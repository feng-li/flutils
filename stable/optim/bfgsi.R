bfgsi <- function(H0,dg,dx) {
### dg is previous change in gradient; dx is previous change in x;
### 6/8/93 version that updates inverse hessian instead of hessian
### itself.
### Copyright by Christopher Sims 1996.  This material may be freely
### reproduced and modified.
  n <- length(dg)
  dim(dg) <- c(n,1)
  dim(dx) <- c(n,1)
  Hdg <- H0 %*% dg
  dgdx <- as.numeric(crossprod(dg,dx))
  dxHdg <- drop(dx %o% Hdg) # drops are needed to get rid of redundant unit-dimensions
  x1 <- as.numeric(1+crossprod(dg,Hdg)/dgdx)*drop(dx %o% dx)
  x2 <- dxHdg+t(dxHdg)
  ## code below causes problems when x1 and x2 have matching zeros, and I can't now (2005-12-22)
  ## figure out why I thought it was a good idea
##   if ( max(abs(x1-x2)/(abs(x1)+abs(x2))) <= 1e-12 ) {
##     cat("bfgs update failed.\n")
##     cat("x1, x2 = ",x1,x2,"\n")
##     H=H0
##     return(H)
##   }
  if (abs(dgdx) <= 1e-12)   {
    cat("bfgs update failed.\n")
    cat("|dg| =", sqrt(sum(dg^2)), "|dx| = ", sqrt(sum(dx^2)),"\n")
    cat("crossprod(dg,dx) =", dgdx,"\n")
    cat("|H*dg| =", crossprod(Hdg),"\n")
    H=H0
    return(H)
  }
  ## otherwise, 
  H <- H0 + x1/dgdx - x2/dgdx
  save(file="H.dat", H)
  return(H)
}
