##' @export
colSds <- function(x)
  {
    out <- apply(x, 2, sd)
    return(out)
  }

##' @export
rowSds <- function(x)
  {
    out <- apply(x, 1, sd)
    return(out)
  }

##' @export
colVars <- function(x)
  {
    out <- apply(x, 2, var)
    return(out)
  }

##' @export
rowVars <- function(x)
  {
    out <- apply(x, 1, var)
    return(out)
  }
