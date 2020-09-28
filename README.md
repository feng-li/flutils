# `flutils`: Feng Li's utility functions written in R

Code structures
---------------

* `R/dbg` `R/dgp` `R/dist` `...`

    Hierarchical structure for functions written in native R.

* `inst/bin`

  Programs under this fold are executable R script. You should be able to run them as
  usual Linux command-line programs. Make sure they are executable using `chmod +x
  embedAllFonts`.

  The help are usually accessible if you run the program with argument `--help`
  e.g. `embedAllFonts --help`.


Installation
------------

The package has a hierarchical code structure that R does not support (see [this
discussion](https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17258)). A `src/Makevars`
file is used to do the the trick.

- **Standard Installation** to install the `master` branch with
  [`devtools`](https://cran.r-project.org/web/packages/devtools/)

    ``` r
    devtools::install_github("feng-li/flutils")
    ```

- **Alternative Installation** to install the `dev` branch

    - Clone the package from GitHub
    ``` sh
    git clone https://github.com/feng-li/flutils.git
    git checkout dev
    ```

    - Now within R
    ``` r
    source("https://raw.githubusercontent.com/feng-li/flutils/master/R/systools/package.flatten.R")
    project.flatten <- package.flatten("/path/to/flutils/")
    devtools::document(project.flatten)
    devtools::install_local(project.flatten, force = TRUE)
    ```

    - Or under Linux system, you can install them to your system with this script.
    ``` sh
    $ ./flutils/inst/bin/install.HS  flutils
    ```

Copyrights
----------

  See the copyright in each individual file.

More information
----------------

* [Author's homepage](http://feng.li/).



Bug reports
-----------

  https://github.com/feng-li/flutils/issues
