##' A simple sequence generater
##'
##' mainly used in the gradient part
##' @title
##' @param init.seq "vecter" the initial sequence without zero
##' @param init.length "vecter" initial circle length,  including zeros
##' @param length.out "integer" how many circles to be generated
##' @return "vecter"
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note First version: ; Current: .
##' @export
mseq <- function(init.seq, init.length, length.out)
{

  nrow0 <- length(init.seq)
  if(init.length<nrow0)
    {
      stop("Not correct sequence setup!")
    }
  seq0 <- matrix(init.seq, nrow0, length.out)
  seq1.tmp <- seq(from = 0, by = init.length, length.out = length.out)
  seq1 <- matrix(seq1.tmp, nrow0, length.out, byrow = TRUE)

  out <- as.vector(seq0+seq1)

  return(out)
}
