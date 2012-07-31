##' Sourcing a bunch of files located from different folders/subfolders.
##'
##' Also provide byte-code compiler before sourcing.
##' @param ... Paths to be sourced
##' @param byte.compile "logical". If TRUE, byte compile the R code first and load. Else,
##'        only load R code.
##' @param recursive "logical". If TRUE, files will be sourced recursively for all
##'        sub folders.
##' @param silent "logical". If TRUE, No detailed information printed out.
##' @param ignore.error "logical". If TRUE, try to continue even errors occur.
##' @return NULL
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @license  GPL(>=2)
##' @note First version: Wed Apr 15 20:00:43 CET 2009;
##'       Current:       Sat Nov 12 15:42:54 CET 2011.
##' TODO: add environmental option, allow to only source byte code.
sourceDir <- function(..., byte.compile = FALSE, recursive = TRUE, silent = FALSE,
                      ignore.error = FALSE)
{
  ## call the path
  Paths <- c(...)
  if(length(Paths) == 0) # if path is not given,  use current working directory
    {
      Paths <- getwd()
    }
  ## Take away the extra separator "\" or "/"
  Paths <- gsub(paste("\\",.Platform$file.sep,"$",sep=""),"",Paths)
  ## Check if byte compile is requested and supported.
  if(byte.compile == TRUE)
    {
      if(version$minor >= 13)
        {
          require("compiler") # require the compiler library
        }
      else
        {
          byte.compile == FALSE
          warning("Byte code compiling not support on this version!")
        }
    }
  n <- 0
  error <- 0
  if(silent == FALSE)
    {
      ## cat("Loading files  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    }
  for(i in 1:length(Paths)) # loop over different paths
    {
      subfiles <- list.files(Paths[i], pattern = "\\.[Rr]$", recursive = recursive) # list
                                        # all subfiles for given pattern at Paths[i]
      if(length(subfiles) == 0) # unknown folder
        {
          warning(paste("\"", Paths[i], "\": no such directory, skipped.", sep = ""))
        }
      else
        {
          if(silent == FALSE)
            {
              cat("\n\"", Paths[i], "/\":\n", sep = "")
            }
        }
      for (nm in subfiles) ##
        {
          n <- n + 1
          path.nm <- file.path(Paths[i], nm)
          if(byte.compile == TRUE)
            {
              compile.try <- try(cmpfile(path.nm), silent = TRUE)
              path2source.nm <- paste(gsub("\\.[Rr]$", "", path.nm), ".Rc", sep = "")
              if(methods::is(compile.try, "try-error") || file.exists(path2source.nm) == FALSE) # non-function file can not be compiled.
                {
                  source(path.nm) # try to use source() directly
                }
              else # load compiled file
                {
                  loadcmp(path2source.nm)
                }
            }
          else # the usual source procedure.
            {
              if(ignore.error == FALSE) # stop at error
                {
                  if(silent == FALSE) # report results
                    {
                      source(path.nm)
                      cat(paste("   \"", nm, "\"", " ... [ok]\n", sep = ""))
                    }
                  else # silent if no error
                    {
                      source(path.nm)
                    }
                }
              else # skip error files
                {
                  try.source <- try(source(path.nm), silent = TRUE)
                  if(silent == FALSE) # report results
                    {
                      if(methods::is(try.source, "try-error"))
                        {
                          cat(paste("   \"", nm, "\"", " ... [??]\n", sep = ""))

                          ## cat(format(c(paste("...\"", nm, "\"", sep = ""), "[??]"), width
                          ##            = 65), "\n")
                          error <- error + 1
                        }
                      else
                        {
                          cat(paste("   \"", nm, "\"", " ... [ok]\n", sep = ""))
                        }
                    }
                  else # silent
                    {
                      if(methods::is(try.source, "try-error"))
                        {
                          error <- error + 1
                        }
                    }
                }
            }
        }
 }
  ## Summary on quit
  if(silent  == FALSE)
    {
      cat(format(paste("[Succeeded files: ",  n - error, ",", " skipped due to error: ",
                       error, ".]\n\n", sep = ""), justify = "centre", width = 65), "\n")
    }
}
