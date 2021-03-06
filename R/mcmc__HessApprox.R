#' Numerical approximation of Hessian
#'
#' Approximation of hessian matrix by using different methods. The gradient
#' should be always a *matrix*. Current method is "outer" for outer
#' product. If the gradient is not an one-column matrix, it will convert it to
#' an one-column matrix.
#' @param gradient "matrix".
#'        p-by-q, the gradient vector/matrix.
#'
#' @param method "character".
#'         Method to be used in the approximation. Possible value may be "outer"
#'         for outer product of gradient.
#'
#' @param matrix "Logical" Should the Hessian matrix be returned or just the
#'         diagonal vector.
#'
#' @references
#'         Villani et al (2009);
#'         Li et al(2010a, b);
#'         Train, K. 2003, Discrete choice methods with simulation(2ed), Cambridge
#'         Univ. Press. p. 193.
#'
#' @author Feng Li, Dept. of Statistics, Stockholm University, Sweden.
#' @note Created: Tue Mar 30 16:33:23 CEST 2010.
#'       Current: Mon Feb 09 18:34:40 CST 2015.
#' @export
hessApprox <- function(gradient, method)
{
  if (tolower(method) =="outer") # outer product of gradient
    {
      grad.length <- length(gradient)
      gradient.vec <- matrix(gradient, grad.length, 1)
      ##    hessian.out <- -tcrossprod(gradient)
      hessian.out <- -gradient^2
    }
  return(hessian.out)
}
