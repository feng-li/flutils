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


install.packages.HS(project)
