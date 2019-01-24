##' Print object.size
##'
##' <details>
##' @title
##' @param x
##' @param unit
##' @return
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Mon Jan 17 20:05:44 CET 2011;
##'       Current:       Mon Jan 17 20:05:51 CET 2011.
##' TODO: allow multiple inputs
##' @export
size <- function(x, unit = "auto")
{
    if(class(x) == "character")
    {
        x <- eval(as.name(x))
    }
    print(object.size(x), unit = unit)
}
