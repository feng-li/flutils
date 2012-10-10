##' Auto update R packages.
##'
##' <details>
##' @title 
##' @param when 
##' @param insOnloc.if.noPrivilege 
##' @param noticeOnly 
##' @return 
##' @references 
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: Thu Apr 21 11:32:34 CEST 2011;
##'       Current:       Thu Apr 21 11:32:40 CEST 2011.
##' TODO: check when two versions installed and skip updade
autoupdate.packages <- function(when, insOnloc.if.noPrivilege = FALSE,
                                noticeOnly = TRUE)
  {
    if(missing(when) || tolower(format(Sys.time(), "%A"))  == tolower(when)) # update
                                        # now or given weekday e.g. Friday
      {
        checkpath <- .libPaths() # available paths
        checkpath.Privilege <- file.access(checkpath, 2) # if writable
        userlib <- Sys.getenv()["R_LIBS_USER"] # user library path with access right        
        whatsnew <- utils::old.packages()

        ## New update available
        if(is.null(whatsnew) == FALSE)
          {
            cat("The following packages are available to update:\n\n")
            cat(whatsnew[, 1], "\n\n")

            if(noticeOnly == FALSE)
              {
                answer <- substr(readline("Update All (yes/no)?  "), 1L, 1L)
                if (tolower(answer) %in% c("yes", "y"))
                  {
                    for(i in 1:length(checkpath)) #loop over all library paths
                      {
                        if(checkpath.Privilege[i] == -1) # not writable
                          {
                            if(insOnloc.if.noPrivilege == TRUE)
                              {
                                instlib <- userlib
                                cat("No privilege in folder, using", instlib, ".\n")
                                ## Sys.sleep(1)
                                utils::update.packages(lib.loc = checkpath[i], ask = FALSE, instlib = instlib)
                              }
                            else
                              {
                                cat("No privilege in folder", checkpath[i], ",  skipped.\n")
                                ## Sys.sleep(1)
                              }
                          }
                        else # writable
                          {
                            utils::update.packages(lib.loc = checkpath[i], ask = FALSE)
                          }
                      }
                  }
                else
                  {
                    cat( "update canceled.\n")
                  }
              }
          }
      } 
  }
