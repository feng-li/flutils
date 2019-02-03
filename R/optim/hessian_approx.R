##' Numerical approximation of hessian matrix
##'
##' Approximation of hessian matrix by using different methods. The gradient should be
##' always a *matrix*. Current method is "outer" for outer product. If the gradient is not
##' an one-column matrix, it will convert it to an one-column matrix.
##'
##' @name hessian_approx
##' @title Numerical approximation of hessian matrix.
##'
##' @param gradient "matrix".
##'        p-by-q, the gradient vector/matrix.
##' @param method "character".
##'         Method to be used in the approximation. Possible value may be "outer"
##'         for outer product of gradient.
##'
##' @return  "matrix". pq-by-pq hessian matrix.
##' @references Villani et al (2009); Li et al(2010a, b);
##'         Train, K. 2003, Discrete choice methods with simulation(2ed), Cambridge
##'         Univ. Press. p. 193.
##'
##' @author Feng Li, Dept. of Statistics, Stockholm University, Sweden.
##' @note First version:  Tue Mar 30 16:33:23 CEST 2010.
##'       Current:        Wed Sep 15 09:45:16 CEST 2010.
##' @export
hessian_approx <- function(gradient,method)
{
  if (tolower(method) =="outer") # outer product of gradient
    {
      dim.grad <- dim(gradient)
      gradient.vec <- matrix(gradient ,prod(dim.grad) ,1)
      hessian.out <- -tcrossprod(gradient)
    }
  else if (tolower(method) =="identity")
    {
        hessian.out = diag(1, nrow = length(gradient))
    }
  else
    {
        stop("No such Hessian approximation method.")
    }
  ## if hessian is bad, then use accurate approximate
  ## if(any(eigen(hessian.out)$values>0))
  ##   {
  ##     ## What should I?
  ##   }
  return(hessian.out)
}
