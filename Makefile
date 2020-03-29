all: install

install:
	$(R_HOME)/bin$(R_ARCH)/Rscript --no-restore --no-save  -e 'flatten=sapply(list.files("R", pattern = "\\.[RrSs]$\", recursive = TRUE, full.names = TRUE), file.copy, "R")'
