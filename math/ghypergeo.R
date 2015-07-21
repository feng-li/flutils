##' The generalized hypergeometric function pFq.
##'
##' The results are evaluated via the series expansion of Pochhammer symbols.
##' @param a "n-by-p matrix"
##' @param b "n-by-q matrix"
##' @param z "positive vector of length one or n"
##' @param k "positive integer"
##'
##'        Indicates how long of the series should be used. The default value
##'        is 10.
##'
##' @return "n-by-1 matrix"
##'
##'       The matrix of length n.
##'
##' @references HypergeometricPFQ in Mathematica 10
##' @author Feng Li, Central University of Finance and Economics.
##' @note Created: Tue Jan 15 12:36:41 CET 2013;
##'       Current: Sat May 30 15:07:07 CST 2015.
ghypergeo <- function(a, b, z, k = 10)
  {
    if(!is.matrix(a)|| !is.matrix(b) ||
       dim(a)[1] != dim(b)[1])
      {
        stop("The arguments `a' and `b' must be matrices \n and should have the same row dimension.")
      }

    a.dim <- dim(a)
    nObs <- a.dim[1]
    a.nCol <-a.dim[2]
    b.dim <- dim(b)
    b.nCol <- b.dim[2]

    n <- k+1
    n.series <- 0:k

    zpower.series <- matrix(n.series, nObs, n, byrow = TRUE)*
      matrix(log(z), nObs, n)

    nfact.series <- lgamma(n.series+1) # log(n!)
    an.series <- pochhammer(t(a), n.series, log = TRUE)
    bn.series <- pochhammer(t(b), n.series, log = TRUE)

    an.ary <- array(t(an.series), c(n, a.nCol, nObs))
    bn.ary <- array(t(bn.series), c(n, b.nCol, nObs))

    an <- apply(an.ary, c(1, 3), sum)
    bn <- apply(bn.ary, c(1, 3), sum)

    nlog <- an - bn + t(zpower.series) - nfact.series
    out <- matrix(apply(exp(nlog), 2, sum))

    return(out)
  }

regghypergeo <- function(a, b, z, k = 10)
{
  ## the regularized hypergeometric function pFq(a, b, z)/Prod[Gamma(b)]
  gammaprod <- apply(gamma(b), 1, prod)
  out <- ghypergeo(a = a, b = b, z = z, k = k)/gammaprod
  return(out)
}
