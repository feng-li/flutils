#' @export
is.positivedefinite <- function(X)
    {
        eig <- eigen(X, only.values = TRUE)$values
        out <- ifelse(all(eig>0), TRUE, FALSE)
        return(out)
    }
