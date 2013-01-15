##' The generalized hyper-geometric function
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
##' @return "vector"
##'
##'       The vector of length n.
##'
##' @references NIST Handbook of Mathematical Functions
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Tue Jan 15 12:36:41 CET 2013;
##'       Current: Tue Jan 15 12:36:57 CET 2013.
ghypergeo <- function(a, b, z, k = 10)
  {
    a.dim <- dim(a)
    nObs <- a.dim[1]
    a.nCol <-a.dim[2]
    b.nCol <- dim(b)[2]

    n <- k+1
    n.series <- 0:k

    zpower.series <- matrix(n.series, nObs, n, byrow = TRUE)*
      matrix(log(z), nObs, n)

    nfact.series <- lgamma(n.series+1) # log(n!)
    an.series <- pochhammer(t(a), n.series, log = TRUE)
    bn.series <- pochhammer(t(b), n.series, log = TRUE)

    an.ary <- array(an.series, c(a.nCol, n, nObs))
    bn.ary <- array(bn.series, c(b.nCol, n, nObs))

    an <- apply(an.ary, c(2, 3), sum)
    bn <- apply(bn.ary, c(2, 3), sum)

    nlog <- an - bn + t(zpower.series) - nfact.series
    out <- apply(exp(nlog), 2, sum)

    return(out)
  }
