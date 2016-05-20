##' Save all the objects to a folder.
##'
##' @param save.output
##' @param ModelDescription
##' @param Starting.time
##' @return NULL
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Sun Sep 02 16:10:36 CEST 2012;
##'       Current: Sun Sep 02 16:10:42 CEST 2012.
save.all <- function(save.output, ModelDescription)
{
  if (save.output != FALSE) ## Save has been requested.
  {
    cat("Saving all outputs in:\n")

    if (save.output  == TRUE)
    {
      save.output <- getwd()
    }

    ## Creat the folder if not exits
    if(file.exists(save.output)  == FALSE)
    {
      dir.create(save.output)
    }
    ## Generate a random phrase to avoid file conflict
    randomPhrase <- rhex(6) ## simple hex character
    outfile <- paste(c(file.path(save.output, ModelDescription),
                       randomPhrase, "Rdata"), collapse =".")

    ## Save all the objects
    ## save.image(file = OUT.file.name, compress = "xz")
    save(list = ls(envir = .GlobalEnv),
         file = outfile,
         envir = .GlobalEnv,
         compress = "xz")


    ## Done message
    cat(paste("\"", outfile, "\"", sep = ""), "\n")
    cat(rep("-", getOption("width")), "\n", sep = "")
  }
}
