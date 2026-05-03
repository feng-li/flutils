#' Clear recorded warnings
#'
#' @param envir Environment from which `last.warning` should be removed.
#' @return `NULL`, invisibly.
#' @export
warningsClear <- function(envir = baseenv())
  {
    assign("last.warning", NULL, envir = envir)
  }
