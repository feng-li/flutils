##' @export
package.flatten <- function(project = NULL)
{

    {if((length(project) == 1L))
     {
         projectHome <- normalizePath(project)
     }
     else if ((length(project) >= 2L))
        {
            stop("Only one argument is accepted.")
        }
     else
        {
            projectHome <- normalizePath(getwd())
        }}

    projectName <- basename(projectHome)
    pkg.tmpdir <- tempdir()
    pkg.tmpdirProject <- file.path(pkg.tmpdir, projectName)
    pkg.tmpdirProjectR <- file.path(pkg.tmpdir, projectName, "R")
    pkg.tmpdirProjectRtmp <- file.path(pkg.tmpdir, projectName, "Rtmp")
    pkg.DESCRIPTION <- read.dcf(file.path(projectHome, "DESCRIPTION"))
    pkg.version <- pkg.DESCRIPTION[, "Version"]
    pkg.Name <- pkg.DESCRIPTION[, "Package"]

    ## Copy project to directory
    if(dir.exists(pkg.tmpdirProject))
    {
        unlink(pkg.tmpdirProject, recursive = TRUE)
    }
    file.copy(projectHome, pkg.tmpdir, overwrite = TRUE, recursive = TRUE)

    # message("Copied project \"", projectName, "\" to",  pkg.tmpdir)

    ## system2("mv", paste(pkg.tmpdirProjectR, pkg.tmpdirProjectRtmp))
    ## system2("mkdir", pkg.tmpdirProjectR)
    file.rename(pkg.tmpdirProjectR, pkg.tmpdirProjectRtmp)
    dir.create(pkg.tmpdirProjectR, recursive = TRUE)

    RtmpFiles = list.files(pkg.tmpdirProjectRtmp, pattern = "\\.[Rr]$", recursive = TRUE)
    RFilesNameNew = gsub(x = RtmpFiles, pattern = "/", replacement="_")

    for(i in 1:length(RtmpFiles))
    {
        from <- file.path(pkg.tmpdirProjectRtmp, RtmpFiles[i])
        to <- file.path(pkg.tmpdirProjectR, RFilesNameNew[i])
        ## system2("mv", paste(from, to))
        file.rename(from, to)

    }
    unlink(pkg.tmpdirProjectRtmp, recursive = TRUE)
    unlink(file.path(pkg.tmpdirProject, ".git"), recursive = TRUE)


    ## system2("R",  paste("CMD check", pkg.tmpdirProject, "-o", pkg.tmpdir), stderr = TRUE)
    ## setwd(pkg.tmpdir)
    ## system2("R",  paste("CMD build", pkg.tmpdirProject))

    pkg.HS <- file.path(pkg.tmpdir, pkg.Name, sep = "")
    ## install.packages(pkg)
    ## system2("mv", paste(pkg, dirname(projectHome)))
    ## file.copy(pkg, dirname(projectHome), overwrite = TRUE)
    ## message("Package \"", basename(pkg), "\" is placed at ", dirname(projectHome))
    message("Project `", projectName, "` is flattened into \n\t", pkg.HS)
    invisible(pkg.HS)
}
