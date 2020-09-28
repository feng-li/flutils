#' Close all graphics devices.
#' @export
close_all <- function()
{
    graphics.off()
    ## try(rgl.quit(), silent = TRUE)
}
