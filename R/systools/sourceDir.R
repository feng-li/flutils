##' sourceDir
##'
##' Sourcing a bunch of files located in different folders/subfolders. Also provide
##' byte-code compiling before sourcing.
##'
##' @param ... "characters"
##'
##'        The inputs can be path to the R code or mixture of paths and files.
##'
##' @param byte.compile "integer".
##'
##'        If byte.compile = 0, the program will only load the R source code.
##'
##'        If byte.compile = 1, the program will first load the byte-compiled file if the
##' byte-compiled file exists and is newer than the R source code, otherwise the program
##' will first byte-compile it and then load the byte-compiled file.
##'
##'        If byte.compile = 2, the program will byte-compile the R source code regardless
##' of the existence of byte-compiled files.
##'
##' @param recursive "logical".
##'
##'        If TRUE, files will be sourced recursively for all sub folders.
##'
##' @param envir "environment".
##'
##'        What is the destiny of the files to be sourced. The default environment is the
##' global environment.
##'
##' @param pattern "string".
##'
##'        What kind of file pattern is sourced.
##'
##' @param ignore.error "logical".
##'
##'        If TRUE, try to continue even errors occur.
##'
##' @return "data.frame".
##'
##'         A summary is returned invisibly if input is not empty, otherwise quit with an
##' error.
##'
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##'
##' @note Initial: Wed Apr 15 20:00:43 CET 2009; Current: Tue Jan 08 23:15:57 CET 2013.
##'
##'       license:  GPL(>=2)
##'
##'       TODO: allow parallel souring.
sourceDir <- function(...,
                      byte.compile = FALSE,
                      recursive = FALSE,
                      envir = .GlobalEnv,
                      pattern = "\\.[Rr]$",
                      ignore.error = FALSE)
{

    ## Save all the input paths as a list
    Paths.in <- c(...)

    ## if path is not given,  use current working directory
    if(length(Paths.in) == 0L)
        {
            Paths.lst <- list(getwd())
        }
    else
        {
            Paths.lst <- as.list(Paths.in)
        }

    ## Check if all inputs are directories. The inputs can also be mixture of
    ## directories and files. If so, split them
    isPathsDir <- unlist(
        lapply(Paths.lst, FUN = function(x) utils::file_test("-d", x)))

    if(any(isPathsDir))
        {
            Paths.dirs <- Paths.lst[isPathsDir]
            Paths.files <- unlist(Paths.in[!isPathsDir])
            ## print(Paths.files)
            ## list all subfiles for given pattern at Paths
            RFiles <- rbind(matrix(unlist(
                lapply(X = Paths.dirs,
                       FUN = list.files,
                       pattern = pattern,
                       recursive = recursive,
                       full.names = TRUE))),
                            Paths.files)
        }
    else
        {
            ## All are files
            RFiles <- matrix(unlist(Paths.lst))
        }

    ## Check if input path is empty, and quit with a warning if so.
    if(length(RFiles) == 0L)
        {
            stop("Nothing found to source. Aborted!")
        }


    ## Check if byte compile is requested and supported.
    if(byte.compile == 1L || byte.compile == 2L)
        {
            ## require the compiler library
            compLoad.try <- try(require("compiler", quietly = TRUE), silent = TRUE)
            if(methods::is(compLoad.try, "try-error"))
                {
                    byte.compile == 0L
                    warning("Byte compiling not supported, switching to usual method.",
                            immediate. = TRUE)
                }
        }


    ## Check if the corresponding compiled files exist
    if(byte.compile == 1L || byte.compile == 2L)
        {
            ## Function that find the modified time of a file
            file.mtime <- function(x) {file.info(x)$mtime}

            ## Get the modification information from the list files
            RFilesInfo <- matrix(apply(RFiles,1,file.mtime))

            RcFilesTest <- matrix(apply(RFiles,1,function(x) paste(x, "c", sep = "")))

            RcFilesExist <- matrix(apply(RcFilesTest,1,file.exists))

            RcFile.mtime <- function(fileTest, fileExist)
                {
                    ifelse(fileExist, file.info(fileTest)$mtime,  0)
                }

            RcFilesInfo <- mapply(FUN = RcFile.mtime,
                                  fileTest = RcFilesTest,
                                  fileExist = RcFilesExist,
                                  USE.NAMES = FALSE)

            ## Check if R file is newer than Rc file
            RcNewer <- (RcFilesInfo > RFilesInfo)
        }
    else
        {
            ## Auxiliary information
            RcNewer <- NA
            RcFilesExist <- NA
            RcFilesTest <- NA
        }

###----------------------------------------------------------------------------
### Write a private source function
###----------------------------------------------------------------------------
    source.prvt <- function(path2R,
                            path2Rc = NA,
                            is.RcNewer = NA,
                            is.RcExist = NA,
                            byte.compile,
                            envir,
                            ignore.error)
        {
            ## The initial status
            success <- TRUE

            if(byte.compile == 0L) # the usual source procedure.
                {
                    source.try <- try(sys.source(path2R, envir = envir),
                                      silent = TRUE)

                    if(ignore.error == FALSE) # stop on error
                        {
                            if(methods::is(source.try,  "try-error"))
                                {
                                    cat(source.try)
                                    stop("Check the above error message on file:\n", path2R)
                                }
                        }
                    else # skip error files
                        {
                            if(methods::is(source.try,  "try-error"))
                                {
                                    success <- FALSE
                                }
                        }
                }
            else if(byte.compile  == 1L || byte.compile  == 2L) # byte compile before sourcing
                {
                    ## If Rc file exists and is newer than R file,  load Rc file
                    ## directly. Otherwise byte compile R file and load it.
                    if(byte.compile  == 1 && is.RcExist && is.RcNewer)
                        {
                            RcOfile <- path2Rc
                        }
                    else
                        {
                            ## Check if the folder is writable, if so, save the compiled file
                            ## there. Otherwise, save it in a temporal directory
                            isWritable <- file.create(path2Rc, showWarnings = FALSE)
                            if(isWritable)
                                {
                                    ## It's OK to write in the same directory
                                    RcOfile <- path2Rc
                                }
                            else
                                {
                                    ## The original path not writable, write to temporal file
                                    path2RcStr <- gsub(pattern = .Platform$file.sep,
                                                       replacement = "_",
                                                       x = path2Rc)
                                    RcOfile <- paste(tempdir(), .Platform$file.sep,
                                                     path2RcStr, sep = "")
                                }

                            ## Byte compile
                            compRc.try <- try(compiler::cmpfile(
                                infile = path2R,
                                outfile = RcOfile,
                                env = envir), silent = TRUE)
                        }

                    source.try <- try(compiler::loadcmp(file = RcOfile, envir = envir),
                                      silent = TRUE)

                    if(methods::is(source.try,  "try-error"))
                        {
                            success <- FALSE
                        }

                }
            else
                {
                    stop("Unrecognized input for argument byte.compile.")
                }
            return(success)
        }

###----------------------------------------------------------------------------
### Apply the source function to the files and output
###----------------------------------------------------------------------------
    sourceSucess <- mapply(FUN = source.prvt,
                           path2R = RFiles,
                           path2Rc = RcFilesTest,
                           is.RcNewer = RcNewer,
                           is.RcExist = RcFilesExist,
                           MoreArgs = list(
                               byte.compile = byte.compile,
                               envir = envir,
                               ignore.error = ignore.error))

    names(sourceSucess) <- NULL

    ## Invisible return the sourcing status
    if(byte.compile == 0L)
        {
            out <- data.frame(RFiles, sourceSucess, stringsAsFactors = FALSE)
        }
    else
        {
            out <- data.frame(RFiles, RcFilesExist, RcNewer, sourceSucess,
                              stringsAsFactors = FALSE)
        }

    if(any(!sourceSucess))
        {
            warning("The following file(s) failed during sourcing:\n\n",
                    paste(RFiles[!sourceSucess, , drop = FALSE], collapse = "\n"),
                    "\n", immediate. = TRUE)
        }

    invisible(out)

}
