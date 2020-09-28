#' Make step size for SGLD
#'
#' Step size is also known as the learning rate in stochastic gradient descent
#' algorithms.
#' @param steprange Range for steps.
#' @param n Total steps.
#' @param args Other arguments
#' @return Steps
#' @author Feng Li, Central University of Finance and Economics.
#' @export
make_stepsize = function(steprange, n, args)
{
    method = args[["method"]]
    if (tolower(method) == "fixed")
    {
        out = rep(steprange[1], n)
    }
    if (tolower(method) == "lin-decay")
    {
        out = seq(steprange[1], steprange[2], length.out = n)
    }
    else if (tolower(method) == "exp-decay")
    {
        lambda = args[["lambda"]]
        out.raw = dexp(seq(qexp(0.01, lambda), qexp(0.99, lambda), length.out = n), lambda)
        out = ((out.raw - min(out.raw)) /(max(out.raw) - min(out.raw)) * (max(steprange)
            - min(steprange)) +min(steprange))
    }
    else
    {
        stop("Not implemented!")
    }
    return(out)
}
