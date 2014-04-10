colSds <- function(x)
  {
    out <- apply(x, 2, sd)
    return(out)
  }

rowSds <- function(x)
  {
    out <- apply(x, 1, sd)
    return(out)
  }

colVars <- function(x)
  {
    out <- apply(x, 2, var)
    return(out)
  }

rowVars <- function(x)
  {
    out <- apply(x, 1, var)
    return(out)
  }
