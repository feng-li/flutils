##' Very Simple progress bar for "for loops"
##'
##' Call this function within for loop. Not work with while loop
##' Have not test with windows Gui
##' @title
##' @param iIter
##' @param nIter
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
##' @export
progressbar <- function(iIter, nIter)
{
    cat("\n")
    setTxtProgressBar(txtProgressBar(min = 0, max = nIter, style = 3), iIter)
    if(iIter == nIter)
    {cat("\n")}
}
