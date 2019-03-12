##' A simple hexadecimal random string generator
##'
##'
##' @title Random hex
##' @param n "positive-integer"
##' @return "string"
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Wed Oct 17 20:04:58 CEST 2012;
##'       Current: Wed Oct 17 20:05:05 CEST 2012.
##' @export
rhex <- function(n)
  {
    hexStr <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                "a", "b", "c", "d", "e", "f")

    hexRnd <- sample(hexStr, n, replace = TRUE)
    out <- paste(hexRnd, collapse = "")
    return(out)
  }
