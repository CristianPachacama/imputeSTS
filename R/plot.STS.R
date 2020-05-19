#' @title Plot of Cleaned and original Time Series
#' @description This function generates a plot with the original and reconstructed series
#' @param STSclean is a object type \code{STS} and It contain imputation of TS time series, by default is \code{NULL}, in that case \code{TSclean = imputeSTS(TS)}
#' @param k is the number of column (time serie) from \code{STSclean} to be plot, by default is \code{NULL} in that case all time series in TS are ploting
#' @return a plot with the original series and the reconstructed series are found
#' @examples
#' x <- co2
#' x[sample(1:length(x),20,FALSE)] <- NA
#' sts <- impute.STS(x)
#' plot(sts)
#' @import ggplot2
#' @import stats
#' @export

plot.STS=function(STSclean,k=NULL){

  TS = STSclean$TS
  TSclean = STSclean$TSclean
  if(!is.null(k) & !is.null(dim(TS))){

    if(k<=dim(TS)[2]){
      TSk=TS[,k]
      TSk_clean = TSclean[,k]
      #........................
      Dates = seq.Date(from = as.Date(paste(c(start(TSk),1),collapse = "/")),
                       to = as.Date(paste(c(end(TSk),1),collapse = "/")),
                       length.out = length(TSk) )
      x=as.data.frame(TSk)
      x$Dates = Dates

      x_cln =as.data.frame(TSk_clean)
      x_cln$Dates = Dates
      # ..............
      p1 = ggplot2::ggplot(data = x_cln,
                           ggplot2::aes_string(x = "Dates", y = "x")) +
        ggplot2::geom_line(size = 1, color="red") +
        ggplot2::geom_line(data = x , size = 1) +
        ggplot2::theme_minimal()
    }else{
      print("Invalid k, choose it lower than number of time series in TS object")
    }

  }

  if(is.null(k) & is.null(dim(TS))){

    TSk=TS
    TSk_clean = TSclean
    #........................
    Dates = seq.Date(from = as.Date(paste(c(start(TSk),1),collapse = "/")),
                     to = as.Date(paste(c(end(TSk),1),collapse = "/")),
                     length.out = length(TSk) )
    x=as.data.frame(TSk)
    x$Dates = Dates

    x_cln =as.data.frame(TSk_clean)
    x_cln$Dates = Dates
    # ..............
    p1 = ggplot2::ggplot(data = x_cln,
                         ggplot2::aes_string(x = "Dates",y = "x")) +
      ggplot2::geom_line(size = 1,color="red") +
      ggplot2::geom_line(data = x , size = 1) +
      ggplot2::theme_minimal()

  }

  if(is.null(k) & !is.null(dim(TS))){

    Dates = seq.Date(from = as.Date(paste(c(start(TS),1),collapse = "/")),
                     to = as.Date(paste(c(end(TS),1),collapse = "/")),
                     length.out = dim(TS)[1] )
    nbts = colnames(TS)
    nbrts = colnames(TSclean)

    x=as.data.frame(TS)
    x$Dates = Dates
    rx=as.data.frame(TSclean)
    rx$Dates = Dates

    plx = reshape2::melt(x,id.vars = "Dates",na.rm = FALSE)
    plx$Tipo="Original"
    plrx= reshape2::melt(rx,id.vars = "Dates",na.rm = FALSE)
    plrx$Tipo="Reconstruidas"

    # ......................
    p1 = ggplot2::ggplot(data = plrx,
                         ggplot2::aes_string(x = "Dates", y = "value")) +
      ggplot2::geom_line(size = 0.8,color="red",
                         # ggplot2::aes(color = variable),
                         # alpha=I(0.3)
                         ) +
      ggplot2::facet_grid(variable~.)+
      ggplot2::geom_line(data = plx , size = 1#,
                         # ggplot2::aes(color = variable)
      ) +
      ggplot2::theme_minimal()
    # ......................
    # p1 = ggplot2::ggplot(data = plrx,
    #                      ggplot2::aes_string(x = "Dates", y = "value")) +
    #   ggplot2::geom_line(size = 0.8,
    #                      ggplot2::aes(color = variable),
    #                      alpha=I(0.3)) +
    #   ggplot2::geom_line(data = plx , size = 1.1,
    #                      ggplot2::aes(color = variable)
    #                      ) +
    #   ggplot2::scale_color_grey(start = 0.1, end = 0.1, na.value= "red") +
    #   ggplot2::theme_minimal()
    #..........................

  }
  # UseMethod("plot")
  return(p1)

}

