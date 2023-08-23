#' Analyze Flood Metrics
#'
#' @description This function analyzes flood metrics based on the provided flood flow data.
#'
#' @param FldFl A dataframe containing the flood flow data.
#' @param i An identifier or index.
#' @param Min_AP Minimum Annual Precipitation for Annual Maximum Series.
#' @param percentile The ratio used in the percentile calculation.
#'
#' @importFrom fitdistrplus fitdist
#' @importFrom lfstat gringorten
#' @importFrom extRemes fevd
#' @importFrom nortest ad.test
#' @importFrom lubridate year
#' @importFrom eva gpdAd pgpd
#' @import eva
#'
#' @return A dataframe with analyzed flood metrics.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe 'FldFl' with flood flow data, an identifier 'i', Min_AP, and percentile:
#' result_df <- analyzeFloodMetrics(FldFl, i, Min_AP, percentile)
#' }
#'
analyzeFloodMetrics <- function(FldFl, i, Min_AP, percentile) {

  # Calculate thres_cfs using the percentile
  Thres_cfs <- quantile(FldFl$Qzpeak, percentile)

  # Call the RP_thres function
  allthres <- RP_thres(i, FldFl, Thres_cfs)

  # Filter for at least one event per year
  allthres2 <- allthres[allthres$NoOfFlood > 83,]

  # Calculation of Mean Residual
  allthres2$meanres <- allthres2$GP_scale / (1 - allthres2$GP_shape)

  #Mean Residual Plot
  plot(allthres2$Thres_cfs,allthres2$meanres)
  abline(lm(meanres ~ Thres_cfs, data = allthres2), col = "blue")

  return(allthres2)
}
