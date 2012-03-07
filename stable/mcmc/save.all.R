##' Save all the objects to a folder.
##'
##' Details.
##' @name 
##' @title 
##' @param path 
##' @param ... 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
save.all <- function(save.output, ModelDescription = NULL, Running.date)
  { 
    if (save.output != FALSE) ## Save has been requested.
      {
        ## Creat the folder if not exits
        if(file.exists(save.output)  == FALSE)
          {
            dir.create(save.output)
          }
        
        OUT.file.name <- paste(file.path(save.output, paste(ModelDescription,
                         format(Running.date, "%Y%b%d-%H.%M.%S"), sep = "_")), ".Rdata", sep = "") 
        
        ## Save all the objects
        save.image(file = OUT.file.name)
        
        ## Save all the figures
        cat("Outputs saved in:\n",  paste("\"", OUT.file.name, "\"", sep = ""), "\n")
        cat("----------------------------------------------------------------------\n\n")
      }
  }
