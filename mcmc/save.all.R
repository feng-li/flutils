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

    JOB_ID <- Sys.getenv("SLURM_JOB_ID")
    if(nchar(JOB_ID)>0)
    {
        JOB.str <- paste("JOB", JOB_ID, ".", sep = "")
    }
    else
    {
        JOB.str <- ""
    }

    outfile <- file.path(save.output,  paste(JOB.str,  ModelDescription,  "." ,
                                             randomPhrase, ".Rdata" ,   sep  =  ""))
    ## Save all the objects
    ## save.image(file = OUT.file.name, compress = "xz")
    cat(paste("\"", outfile, "\"", sep = ""), "\n")

    save(list = ls(envir = .GlobalEnv),
         file = outfile,
         envir = .GlobalEnv)


    ## Done message
    cat(rep("-", getOption("width")), "\n", sep = "")
  }
}
