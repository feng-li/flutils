##' Transform the mean function in the GLM framework.
##'
##' @title GLM mean function
##' @param X "matrix" 
##' @param beta "one-col-matrix" 
##' @param link "character" Type of link function
##' @return "one-col-matrix" of the same dimension as the linear predictor
##' @references NA
##' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
##' @note Created: Sun Mar 04 14:45:22 CET 2012;
##'       Current: Sun Mar 04 14:45:29 CET 2012.
parMeanFun <- function(X, beta, link)
  {
    ## Input each observation x'b  -> l(phi) = x'b -> phi
    beta <- matrix(beta)
    linPred <- X %*% beta # The linear predictor
    
    
    if(tolower(link) == "identity")
      {
        out <- linPred
      }
    else if(tolower(link) == "log")
      {
        out <- exp(linPred)
      }
    else if(tolower(link) == "logit")
      {
        out <- 1/(1+exp(-linPred))
      }
    else
      {
        stop("This link function is not implemented yet!")
      }
    
    return(out)
  }
