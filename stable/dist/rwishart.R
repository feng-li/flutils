##' Description
##'
##' Details.
##' @name 
##' @title 
##' @param nu 
##' @param V 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
rwishart <- function(nu,V)
{
##
## function to draw from Wishart (nu,V) and IW
## 
## W ~ W(nu,V)
## E[W]=nuV
##
## WI=W^-1
## E[WI]=V^-1/(nu-m-1)
## 
## Came from package "bayesm",  Peter Rossi
  
  m <- nrow(V)
  df <- (nu+nu-m+1)-(nu-m+1):nu
  if(m >1)
    {
      T <- diag(sqrt(rchisq(c(rep(1,m)),df)))
      T[lower.tri(T)] <- rnorm((m*(m+1)/2-m))
    }
  else
    { T=sqrt(rchisq(1,df)) }
  
  U <- chol(V)
  C <- t(T)%*%U
  CI <- backsolve(C,diag(m))
                                        #
                                        #   C is the upper triangular root of Wishart
                                        #      therefore, W=C'C  this is the LU decomposition 
                                        #      Inv(W) = CICI'  Note:  this is the UL decomp not LU!
                                        #
  return(list(W=crossprod(C),IW=crossprod(t(CI)),C=C,CI=CI))
                                        #  W is Wishart draw,  IW is W^-1
}

##' Description
##'
##' Details.
##' @name 
##' @title 
##' @param df 
##' @param V 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
riwishart <- function(df, V)
  {
    rwishart(nu = df, V = solve(V))$IW
  }
