#' Test positive definiteness
#'
#' @param X A square numeric matrix.
#' @return `TRUE` if all eigenvalues are strictly positive; otherwise `FALSE`.
#' @export
is.positivedefinite <- function(X)
    {
        eig <- eigen(X, only.values = TRUE)$values
        out <- ifelse(all(eig>0), TRUE, FALSE)
        return(out)
    }
