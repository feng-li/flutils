Feng Li's collection of R utility functions
===========================================

  This folder contains the R utility functions written by Feng Li.

Code structures
---------------

* `R/dbg` `R/dgp` `R/dist` ...

    Hierarchical structure for functions written in native R.

* `inst/bin`

  Programs under this fold are executable R script. You should be able to run them as
  usual Linux command-line programs. Make sure they are executable using `chmod +x
  embedAllFonts`.

  The help are usually accessible if you run the program with argument `--help`
  e.g. `embedAllFonts --help`.


Installation
------------

The package has a hierarchical code structure that standard R does not support (see [this
discussion](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17258)). You have to
follow the steps to install it manually.

- Clone the package from GitHub

        $ git clone git@github.com:feng-li/flutils.git

- Now within R

        source("https://raw.githubusercontent.com/feng-li/flutils/master/R/systools/package.flatten.R")
        project.flatten <- package.flatten("/path/to/flutils/")
        devtools::document(project.flatten)
        devtools::install_local(project.flatten, force = TRUE)

        Or under Linux system, you can install them to your system with this script.

        $ ./flutils/R/systools/install.HS flutils

TODO
====

Documentations are not well presented.

Copyrights
---------

  See the copyright on each individual file.

More information
----------------

* [Author's homepage](http://feng.li/).



Bug reports
-----------

  https://github.com/feng-li/flutils/issues
