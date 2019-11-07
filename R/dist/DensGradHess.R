#' A collection of gradient and Hessian for the log form of common densities.
#'
#' The parameters after "..." should be matched exactly.
#' @param B "matrix".  The paramter that need to be added with a prior. The gradient and
#'     hessian are calculated conditional on B. B should be always an one-column matrix
#'
#' @param ... The arguments for the densities. The name should be matched exactly.
#' @param type density type
#' @param grad "logical" Should the gradient be computed?
#' @param Hess "logical" Should the Hessian be computed?
#' @return "list". The gradient and Hessian (if required) matrices, see below. grad is an
#'     one-column matrix; Hess is a squared matrix whose dimension on row and column are
#'     same as length of B
#'
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
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
