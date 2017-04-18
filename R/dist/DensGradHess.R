##' A collection of gradient and Hessian for the log form of common densities.
##'
##' The parameters after "..." should be matched exactly.
##' @param B "matrix".  The paramter that need to be added with a prior. The
##' gradient and hessian are calculated conditional on B. B should be always an
##' one-column matrix
##'
##' @param ... The arguments for the densities. The name should be matched
##' exactly.
##' @param type
##' @param grad "logical" Should the gradient be computed?
##' @param Hess "logical" Should the Hessian be computed?
##' @return "list". The gradient and Hessian (if required) matrices, see below.
##' \item   {grad}
##'         {"matrix". One-column.}
##'
##' \item   {Hess}
##'         {"matrix". A squared matrix. Dimension on row and column are  same
##'         as length of B.}
##'
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Tue Mar 30 09:33:23 CEST 2010;
##'       Current:       Fri Mar 02 17:01:20 CET 2012.
DensGradHess <- function(B, ..., type = "norm", grad = TRUE, Hess = TRUE)
{
    parArgs <- list(...)
    out <- list()

    if(ncol(B) != 1)
        {
            stop("B must be a column-matrix!")
        }

    if (tolower(type) == "norm") # vecB ~ N(mean, shrinkage*covariance)
        {
            mean <- parArgs$mean
            covariance <- parArgs$covariance


            CovInv <- solve(covariance)

            ## The gradient
            if(grad == TRUE)
                {
                    out[["grad"]] <- -CovInv %*% (B-mean)
                }
            ## The Hessian
            if(Hess == TRUE)
                {
                    out[["Hess"]] <- -CovInv
                }
        }

    return(out)
}
