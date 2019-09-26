##' List all attached packages
##'
##'
##' @title list all attached packages
##' @return NA
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Wed Mar 21 12:01:21 CET 2012;
##'       Current: Wed Mar 21 12:01:27 CET 2012.
##' @export
attached.packages <- function()
{
  searchOut <- search()
  loadedNMS <- loadedNamespaces()
  out <- searchOut[searchOut%in%paste("package:", loadedNMS ,sep="")]
  return(out)
}
