##' View PDF document
##'
##' <details>
##' @title 
##' @param reader 
##' @param delete 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
## vp <- function(topic, reader = getOption("pdfviewer"), delete = TRUE)
##   {
##     help0 <- help(topic = topic, help_type = "pdf")
##     pdffile <- paste(topic, ".pdf", sep = "")
##     caller <- paste(reader, pdffile)
##     call0 <- system(caller)

##     if(delete == TRUE)
##       {
##         file.remove(pdffile)
##       }
##   }


## viewpdfhelp <- function(topic, reader = getOption("pdfviewer"))
##   {
##     help(topic = topic, help_type = "pdf") # save pdf file
##     pdffile <- paste(topic, ".pdf", sep = "") # the pdf file name
##     system(paste(reader, pdffile)) #call the viewer     
##   }

## viewpdfhelp("par")
