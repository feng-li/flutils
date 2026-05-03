#' Row and column summaries
#'
#' Convenience wrappers around `apply()` for row and column standard deviations
#' and variances.
#'
#' @param x A numeric matrix.
#' @return A numeric vector.
#' @name matrix-summaries
#' @export
colSds <- function(x)
  {
    out <- apply(x, 2, sd)
    return(out)
  }

#' @rdname matrix-summaries
#' @export
rowSds <- function(x)
  {
    out <- apply(x, 1, sd)
    return(out)
  }

#' @rdname matrix-summaries
#' @export
colVars <- function(x)
  {
    out <- apply(x, 2, var)
    return(out)
  }

#' @rdname matrix-summaries
#' @export
rowVars <- function(x)
  {
    out <- apply(x, 1, var)
    return(out)
  }
