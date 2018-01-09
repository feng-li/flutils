Feng Li's collection of R utility functions
===========================================

  This folder contains the R utility functions which are depended by other programs.

Structure of the folders
------------------------

* sid

  Code under this folder is still in development which is not suitable for
  productive environment.

* bin

  Programs under this fold are executable R script. You should be able to run
  them as usual Linux command-line programs. Make sure they are executable
  using `chmod +x embedAllFonts`.

  The help are usually accessible if you run the program with argument `--help`
  e.g. `embedAllFonts --help`.


Installation
------------

  You can either load the full library with the following commands with R

    R > source("flutils/R/systools/sourceDir.R")
    R > sourceDir("flutils/R", recursive = TRUE)

  Under Linux system, you can install them to your system with this script (But note that
  the functions are not exported to the namespace at the moment. You have to use
  `flutils::foo()` to call `foo()` in R)

    $ ./flutils/R/systools/install.HS.R


Copyright
---------

  See the copyright on each individual file.

More information
----------------

* [Author's homepage](http://feng.li/).

* [Private repository](https://bitbucket.org/fli/).



Bug reports
-----------

  Contact Feng Li <feng.li@cufe.edu.cn>
