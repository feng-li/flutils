##' Add grids on plot
##'
##' Better than grid() function
##' @title 
##' @param x.at 
##' @param y.at 
##' @param col 
##' @param lty 
##' @param lwd 
##' @param ... 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
grid2 <- function(x.at = NA, y.at = NA, col = "black", lty="dotted", lwd = 0.5, ...)
  {
    abline(h = y.at, v = x.at, col = col, lty = lty, lwd = lwd, ...)
  }
