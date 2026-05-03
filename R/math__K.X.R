#' Multiply by a commutation matrix
#'
#' Reorder rows or columns as if multiplying by `K(m, n)`, without forming the
#' dense commutation matrix explicitly.
#'
#' @param m,n Positive integer dimensions of the commutation matrix.
#' @param X A dense matrix to be multiplied.
#' @param t Logical; if `FALSE`, left-multiply `X` by `K(m, n)`; otherwise
#'   right-multiply `X` by `K(m, n)`.
#' @return A reordered matrix.
#' @export
K.X <- function(m,n,X,t)
{
  if(t==FALSE)
   {
    if(dim(X)[1]!=(m*n))
      {stop("The dimensions of (K %*% X) are not conformable.")}
     key <- as.vector(t(matrix(c(1:(m*n)),m,n)))
     X <- X[key,, drop = FALSE]
    }
  else
    {
     if(dim(X)[2]!=(m*n))
      {stop("The dimensions of (X %*% K) are not conformable")}
      key <- as.vector(t(matrix(c(1:(m*n)),n,m)))
      X<- X[,key, drop = FALSE]
    }
  return(X)
}
