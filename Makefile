PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

all: flatten docs install

flatten:
	Rscript --no-restore --no-save  -e 'flatten=sapply(list.files("R", pattern = "\\.[RrSs]$\", recursive = TRUE, full.names = TRUE), file.copy, "R")'

docs:
	Rscript --no-restore --no-save -e 'devtools::document(".")'

install:
	R CMD INSTALL .
