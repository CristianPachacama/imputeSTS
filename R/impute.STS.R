#' @title Imputation of Seasonal Time Series
#' @description This function aproximate by interpolation the values of a time series (unknonw), based in the values of near (geographically) time series.
#' @param TS is a object of class \code{ts}, time series (either \code{ncol()>=2})
#' @param seasonality either the character string "periodic" or the span (in lags) of the loess window for seasonal extraction
#' @param trend he span (in lags) of the loess window for trend extraction, which should be odd. If \code{NULL}, the default, \code{nextodd(ceiling((1.5*period) / (1-(1.5/s.window))))}, is taken.
#' @param ... Extra arguments passed to \code{stlplus} function from \code{stlplus} package from "https://github.com/hafen/stlplus"
#' @return returns an object of class "ts", containing the reconstructed time series
#' @examples
#' # Univariate Time Serie imputation
#' x <- co2
#' x[sample(1:length(x),30,FALSE)] <- NA
#' sts <- impute.STS(x)
#'
#' # Multivariate Time Serie imputation
#' z <- rnorm(300)
#' z[sample(1:length(z),60,FALSE)] <- NA
#' z <- sin(1:length(z)) + z
#' z <- matrix(z, 100, 3)
#' z <- ts(z, start = c(1961, 1), frequency = 12)
#' sts <- impute.STS(z)
#' @import stlplus
#' @import stats
#' @export

impute.STS = function(TS, seasonality=12,trend =NULL, ...){

  if(!is.null(dim(TS))){

    n=dim(TS)[2]
    lambda=1
    TSclean = matrix(NA,nrow = dim(TS)[1],ncol = n)
    for(k in 1:n){
      TSk=TS[,k]

      if(sum(is.na(TS))>0){
        descom= stlplus::stlplus(TSk,s.window = seasonality,t.window=trend, ...)

        Tren=descom$data$trend
        Seas=descom$data$seasonal
        Res=descom$data$remainder

        Res[is.na(Res)] = simu.STS(Xrand = Res, n=sum(is.na(Res)))
        TScleank=Tren+Seas+Res

      } else {
        TScleank=TSk
      }
      TSclean[,k] = TScleank
    }#end for
    TSclean=data.frame(TSclean)
    names(TSclean) = colnames(TS)
    TSclean = ts(TSclean,start = start(TS),frequency = frequency(TS))

  }else{

    if(sum(is.na(TS))>0){
      descom= stlplus::stlplus(TS,s.window = seasonality,t.window=trend, ...)

      Tren=descom$data$trend
      Seas=descom$data$seasonal
      Res=descom$data$remainder

      Res[is.na(Res)] = simu.STS(Xrand = Res, n=sum(is.na(Res)))
      TSclean = Tren+Seas+Res
      TSclean = ts(TSclean,start = start(TS),frequency = frequency(TS))

    } else {
      TSclean=TS
    }
  }
  STSclean = list("TSclean" = TSclean, "TS"=TS)
  class(STSclean) = "STS"
  return(STSclean)
}

