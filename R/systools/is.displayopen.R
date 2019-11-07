#' Is display open or not.
#'
#' A simple way to check if the display is available.
#' @title is.displayopen
#' @return TRUE/FALSE
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
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
    else if(tolower(sysname) == "windows")
      {
        out <- TRUE
      }

    return(out)
  }
