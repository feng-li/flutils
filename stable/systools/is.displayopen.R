##' Is display open or not.
##'
##' A simple way to check if the display is available.
##' @title is.displayopen
##' @return TRUE/FALSE
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Tue Jul 31 17:35:51 CEST 2012;
##'       Current: Tue Jul 31 17:35:57 CEST 2012.
is.displayopen <- function()
  {
    out <- NA

    sysname <- Sys.info()["sysname"]

    if(tolower(sysname) == "linux")
      {
        display <- Sys.getenv("DISPLAY")
        if(length(display) == 0)
          {
            out <- FALSE
          }
        else
          {
            out <- TRUE
          }
      }
    else if(tolower(system) == "windows")
      {
        out <- TRUE
      }

    return(out)
  }
