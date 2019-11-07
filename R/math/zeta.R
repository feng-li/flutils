## This is the R implementation of the Riemann zeta function.
## NOTE: Note so well construct.
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
