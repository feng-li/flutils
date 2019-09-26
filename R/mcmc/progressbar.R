##' Very Simple progress bar for "for loops"
##'
##' Call this function within for loop. Not work with while loop
##' Have not test with windows Gui
##' @param iIter Current iteration
##' @param nIter Total iteration
##' @return NULL
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @export
progressbar <- function(iIter, nIter)
{
    cat("\n")
    setTxtProgressBar(txtProgressBar(min = 0, max = nIter, style = 3), iIter)
    if(iIter == nIter)
    {cat("\n")}
}
