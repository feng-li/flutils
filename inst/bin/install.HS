#! /usr/bin/env Rscript
project <- commandArgs(TRUE);if(!interactive()&&("--help" %in% project)){cat("
NAME: install.HS - build and install R package with subdirectories

DESCRIPTION:

    An R script that can convert a project with hierarchical structure to
    standard R package structure and install it.

USAGE:

    $ ./install.HS <project>

    $ ./install.HS --help

    R> install.packages.HS('project')

SEE ALSO:

    flutils/systools/sourceDir()

");q("no")}
if(("flutils" %in% installed.packages()))
{
    project.flatten <- try(flutils::package.flatten(project), silent = TRUE)
}
if(is(project.flatten, "try-error"))
{
    source("https://raw.githubusercontent.com/feng-li/flutils/master/R/systools/package.flatten.R")
    project.flatten <- package.flatten(project)
}
devtools::document(project.flatten)
devtools::install_local(project.flatten, force = TRUE)
