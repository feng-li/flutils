##' Close all graphics devices.
##' @export
close.all <- function()
{
    graphics.off()
    ## try(rgl.quit(), silent = TRUE)
}
