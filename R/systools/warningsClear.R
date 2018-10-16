##' @export
warningsClear <- function(envir = baseenv())
  {
    assign("last.warning", NULL, envir = envir)
  }
