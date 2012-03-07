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
##' @note Created: ; Current: .
parLinkFun <- function(mu, link)
  {
    ## Input each observation x'b  -> l(phi) = x'b -> phi
    
    ## linPred <- X %*% beta # The linear predictor
    
    
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
    else
      {
        stop("This link function is not implemented yet!")
      }    
    return(out)
  }
