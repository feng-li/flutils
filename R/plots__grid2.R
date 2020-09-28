#' Add grids on plot
#'
#' Better than grid() function with abline() implementation
#' @param x.at  position on x-axis
#' @param y.at position on y-axis
#' @param col color
#' @param lty line type
#' @param lwd line width
#' @param ... other arguments passed to abline()
#' @return NA
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
grid2 <- function(x.at = NA, y.at = NA, col = "black", lty="dotted", lwd = 0.5, ...)
  {
    abline(h = y.at, v = x.at, col = col, lty = lty, lwd = lwd, ...)
  }
