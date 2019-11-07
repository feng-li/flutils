#' Pochhammer symbol
#'
#' The Pochhammer symbol is evaluated via the expression (a)n  =
#' gamma(a+n)/gamma(a) for positive integer n.
#' @param a "vector".
#' @param n "positive integer vector".
#' @param log "logical"
#'
#'        If TRUE, return the logarithm of Pochhammer symbol. Else return the
#' usual one.
#'
#' @return "matrix"
#'
#'        The (i, j)th element of the matrix indicates the Pochhammer symbol
#' for (a[i])n[j].
#'
#' @references NA
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @note Created: Tue Jan 15 11:20:51 CET 2013;
#'       Current: Tue Jan 15 11:21:00 CET 2013.
#' @export
pochhammer <- function(a, n, log)
  {
    a.len <- length(a)
    n.len <- length(n)

    a.mat <- matrix(a, a.len, n.len)
    n.mat <- matrix(n, a.len, n.len, byrow = TRUE)
    out.log <- lgamma(a.mat+n.mat) - lgamma(a.mat)

    if(log)
      {
        out <- out.log
      }
    else
      {
        out <- exp(out.log)
      }

    return(out)
  }
