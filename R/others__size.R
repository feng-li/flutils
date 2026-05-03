#' Print object.size
#'
#' Object size in a human-readable way.
#' @param x object name
#' @param unit the unit to use. Auto default.
#' @return size
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
size <- function(x, unit = "auto")
{
    if(is.character(x))
    {
        x <- eval(as.name(x))
    }
    print(object.size(x), unit = unit)
}
