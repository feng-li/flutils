#' Approximate Riemann zeta function
#'
#' Evaluate a finite-series approximation to the Riemann zeta function.
#'
#' @param s Numeric vector of powers.
#' @param k Positive integer number of terms in the finite sum.
#' @return Numeric vector with the same length as `s`.
#' @export
zeta <- function(s, k = 10)
  {
    sVec <- as.vector(s)
    nObs <- length(s)
    kSeries <- 1:k
    kMat <- matrix(1/kSeries, nObs, k)
    out <- rowSums(kMat^sVec)
    return(out)
  }
