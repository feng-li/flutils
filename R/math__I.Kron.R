#' Block diagonal Kronecker product with an identity matrix
#'
#' Construct `kronecker(diag(n), X)`.
#'
#' @param n A positive integer.
#' @param X A matrix.
#' @return A block diagonal matrix with `n` copies of `X`.
#' @export
I.Kron <- function(n,X)
{
   X.dim <- dim(X)
   m0 <- X.dim[1]
   n0 <- X.dim[2]
   X.tmp <- matrix(0,n*m0,n*n0)

   for(i in 1:n)
     {X.tmp[(1+m0*(i-1)):(m0*i) ,(1+n0*(i-1)):(n0*i) ] <- X }
   ## TODO: remove the loops
   return(X.tmp)
 }
