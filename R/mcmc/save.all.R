#' Save all the objects to a folder.
#'
#' @param save.output If the output should save.
#' @param ModelDescription String
#' @param Starting.time Computing initialized time
#' @return NULL
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @export
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

    JOB_ID <- Sys.getenv("SLURM_JOB_ID")
    if(nchar(JOB_ID)>0)
    {
        JOB.str <- paste("JOB", JOB_ID, ".", sep = "")
    }
    else
    {
        JOB.str <- ""
    }

    outfile <- file.path(save.output,  paste(JOB.str,  ModelDescription, ".Rdata" ,
                                             sep  =  ""))
    ## Save all the objects
    ## save.image(file = OUT.file.name, compress = "xz")
    cat(paste("\"", outfile, "\"", sep = ""), "\n")

    save(list = ls(envir = .GlobalEnv),
         file = outfile,
         envir = .GlobalEnv)

    ## Done message
    cat(rep("-", getOption("width")), "\n", sep = "")
    invisible(outfile)

  }
}
