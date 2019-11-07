#' The reparameterized beta distribution. This is a wrapper of the usual beta functions.
#'
#' See the help for general information of beta distribution. Note that the
#' mean and standard deviation should be in (0, 1) and (0, mean(1-mean)),
#' respectively.
#' @title The reparameterized beta distribution
#' @param mean "numeric" The mean value of beta distribution.
#' @param sd  "numeric" The standard deviation of beta distribution.
#' @param cond.warning "logical" If warning should be printed if NA produced.
#' @return See the return of beta distribution.
#' @references Li,  2012
#' @author Feng Li, Department of Statistics, Stockholm University, Sweden.
#' @note Created: Tue Apr 10 19:15:06 CEST 2012;
#'       Current: Tue Apr 10 19:15:11 CEST 2012.
#' @export
rbeta2 <- function(n, mean, sd, ncp = 0, cond.warning = TRUE)
  {
    mu <- mean
    sigma2 <- sd^2

    shape1 <- -mu*(mu^2-mu+sigma2)/sigma2
    shape2 <- -1+mu+(mu-1)^2*mu/sigma2

    suppressWarnings(
      out <- rbeta(n = n, shape1 = shape1, shape2 = shape2, ncp = ncp))
    if(cond.warning && any(is.na(out)))
      {
        warning("NA produced. Mean and sd^2 should be in (0, 1) and (0, mean(1-mean)),  respectively.")
      }
    return(out)
  }
pbeta2 <- function(q, mean, sd, ncp = 0, lower.tail=TRUE,
                   log.p = FALSE, cond.warning = TRUE)
  {
    mu <- mean
    sigma2 <- sd^2

    shape1 <- -mu*(mu^2-mu+sigma2)/sigma2
    shape2 <- -1+mu+(mu-1)^2*mu/sigma2

    suppressWarnings(
      out <- pbeta(q, shape1 = shape1, shape2 = shape2, ncp = ncp,
                   lower.tail=lower.tail, log.p = log.p))
    if(cond.warning && any(is.na(out)))
      {
        warning("NA produced. Mean and sd^2 should be in (0, 1) and (0, mean(1-mean)),  respectively.")
      }

    return(out)
  }
qbeta2 <- function(p, mean, sd, ncp = 0, lower.tail=TRUE,
                   log.p = FALSE, cond.warning = TRUE)
  {
    mu <- mean
    sigma2 <- sd^2

    shape1 <- -mu*(mu^2-mu+sigma2)/sigma2
    shape2 <- -1+mu+(mu-1)^2*mu/sigma2

    suppressWarnings(
      out <- qbeta(p, shape1 = shape1, shape2 = shape2, ncp = ncp,
                   lower.tail=lower.tail, log.p = log.p))
    if(cond.warning && any(is.na(out)))
      {
        warning("NA produced. Mean and sd^2 should be in (0, 1) and (0, mean(1-mean)),  respectively.")
      }

    return(out)
  }
dbeta2 <- function(x, mean, sd, ncp = 0,
                   log = FALSE, cond.warning = TRUE)
  {
    mu <- mean
    sigma2 <- sd^2

    shape1 <- -mu*(mu^2-mu+sigma2)/sigma2
    shape2 <- -1+mu+(mu-1)^2*mu/sigma2

    suppressWarnings(
      out <- dbeta(x, shape1 = shape1, shape2 = shape2, ncp = ncp,
                   log = log))
    if(cond.warning && any(is.na(out)))
      {
        warning("NA produced. Mean and sd^2 should be in (0, 1) and (0, mean(1-mean)),  respectively.")
      }

    return(out)
  }
