##' Close all graphics devices.
close.all <- function()
  {
    graphics.off()
    ## try(rgl.quit(), silent = TRUE)
  }
