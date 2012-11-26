##' Transform the mean function in the GLM framework.
##'
##' <details>
##' @title <short tile>
##' @param X "matrix"
##' @param beta "one-col-matrix"
##' @param link "character" Type of link function
##' @return "one-col-matrix" of the same dimension as the linear predictor
##' @references
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Thu May 10 13:09:08 CEST 2012;
##'       Current: Thu May 10 13:09:14 CEST 2012.
parLinkFun <- function(mu, link, extArgs = NA)
  {
    ## Input each observation x'b  -> l(phi) = x'b -> phi

    ## output: The linear predictor linPred =  X %*% beta

    if(tolower(link) == "identity")
      {
        out <- mu
      }
    else if(tolower(link) == "log")
      {
        out <- log(mu)
      }
    else if(tolower(link) == "logit")
      {
        out <- log(mu) - log(1-mu)
      }
    else if(tolower(link) == "glogit" )
      {
        ## The generalized logit link
        ## The logit link is a special case when a  =  0 and b  =  1.
        a <- extArgs$a
        b <- extArgs$b
        out <- log(mu-a) - log(b-mu)
      }
    else
      {
        stop("This link function is not implemented yet!")
      }
    return(out)
  }
