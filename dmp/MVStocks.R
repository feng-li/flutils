##' Multivariate Variables for Stocks
##'
##' Download and build multivariate responses and covariates from Yahoo Finance.
##' @param from integer format as YYYYMMDD
##'
##' @param to integer format as YYYYMMDD
##'
##' @param stocks a vector of characters. The aberrations are same as stocks used in
##' Yahoo! Finance.
##'
##' @param StdDataMethod character. Whether the data should be standardized.
##'
##' @param save2diskPath Is not missing, the result will be saved to the path provided
##' here.
##'
##' @param ... Other arguments passed to stock2covarites
##'
##' @return list or save to disk.
##'
##' @references Li Villani 2012
##'
##' @author Feng Li, Central University of Finance and Economics.
MVStocks <- function(from, to, stocks = c("^SML", "^OEX"),
                     StdDataMethod, save2diskPath, ...)
{

  ## Format the URL in Yahoo URL Format as
  ## http://real-chart.finance.yahoo.com/table.csv?s = %5ESML&a = 07&b = 2&c = 1982&d = 11&e = 21&f = 2014&g = d&ignore = .csv
  a <- substr(from, 7, 8)
  b <- as.numeric(substr(from, 5, 6))-1
  c <- substr(from, 1, 4)
  d <- substr(to, 7, 8)
  e <- as.numeric(substr(to, 5, 6))-1
  f <- substr(to, 1, 4)

  g <- "d" ## daily

  ## THE SOURCE OF DATA
  RawDataPath <- list()
  for(s in stocks)
  {
    RawDataPath[[s]] <- paste("http://real-chart.finance.yahoo.com/table.csv?s=",s,
                              "&a=",a,"&b=",b,"&c=",c,
                              "&d=",d,"&e=",e,"&f=",f,
                              "&g=",g,"&ignore=.csv", sep = "")
  }

  ## Convert the raw data into covariates
  Scovar <- lapply(X = RawDataPath, FUN = stock2covariates, ...)

  XRaw <- list()
  YRaw <- list()

  ## Extract common IDs. Due to technical reasons, the stocks are not available for some
  ## days. We extract the common days.

  ScovarID <- lapply(Scovar, function(x)x[["ID"]])
  ID <- Reduce(intersect, ScovarID)

  for(s in stocks)
  {
    XRaw[[s]] <- Scovar[[s]][["X"]][ID, , drop = FALSE]
    YRaw[[s]] <- Scovar[[s]][["Y"]][ID, , drop = FALSE]
  }

  ## Standardize the data
  if(!missing(StdDataMethod))
  {
    XNew <- lapply(XRaw, StdData, method = StdDataMethod)
    YNew <- lapply(YRaw, StdData, method = StdDataMethod)

    X <- lapply(XNew, function(x) x$data)
    Y <- lapply(YNew, function(x) x$data)
    X.config <- lapply(XNew, function(x) x$config)
    Y.config <- lapply(YNew, function(x) x$config)
  }    else
  {
    X <- XRaw
    Y <- YRaw
    X.config <- NA
    Y.config <- NA
  }

  ## Save to file
  if(!missing(save2diskPath))
  {
    save(ID, X, Y,  X.config, Y.config, file = save2diskPath, compress = "xz")
  }
  else
  {
    return(list(ID, X, Y, X.config, Y.config))
  }

}
