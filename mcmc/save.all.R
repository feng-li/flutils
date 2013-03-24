##' Save all the objects to a folder.
##'
##' @param save.output
##' @param ModelDescription
##' @param Starting.time
##' @return NULL
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Sun Sep 02 16:10:36 CEST 2012;
##'       Current: Sun Sep 02 16:10:42 CEST 2012.
save.all <- function(save.output, ModelDescription = NULL, Starting.time)
  {
    if (save.output != FALSE) ## Save has been requested.
      {
        ## Creat the folder if not exits
        if(file.exists(save.output)  == FALSE)
          {
            dir.create(save.output)
          }

        ## Generate a random phrase to avoid file conflict
        randomPhrase <- rhex(6) ## simple hex character
        OUT.file.name <- paste(
            file.path(save.output,
                      paste(ModelDescription,
                            format(Starting.time, "%Y%b%d-%H.%M.%S"),
                            sep = "_")),
            "_", randomPhrase, ".Rdata", sep = "")

        ## Save all the objects
        save.image(file = OUT.file.name)

        ## Done message
        cat("Outputs saved in:\n",  paste("\"", OUT.file.name, "\"", sep = ""), "\n")
        cat("----------------------------------------------------------------------\n\n")
      }
  }
