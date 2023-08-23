#' Calculate Flood Metrics for Various Thresholds
#'
#' @description This function calculates flood metrics for a sequence of thresholds.
#'
#' @param i An identifier or index.
#' @param FldFl A dataframe containing the flood flow data.
#' @param Min_AP Minimum Annual Precipitation for Annual Maximum Series.
#'
#' @importFrom fitdistrplus fitdist
#' @importFrom lfstat gringorten
#' @importFrom extRemes fevd
#' @importFrom eva pgpd
#' @importFrom nortest ad.test
#' @importFrom lubridate year
#' @importFrom eva gpdAd
#'
#' @return A dataframe with flood metrics calculated for various thresholds.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe 'FldFl' with flood flow data, an identifier 'i', and Min_AP:
#' result_df <- RP_thres(i, FldFl, Min_AP)
#' }
#'
RP_thres <- function(i, FldFl, Min_AP){
  # Define a sequence of thresholds for analysis
  X_thres <- seq(0, 0.95, 0.05)

  # Initialize an empty dataframe to store results
  allthres <- data.frame()

  # Loop through each threshold and compute results using GenParPois function
  for (thres in X_thres){
    stnzz <- GenParPois(i, FldFl, Min_AP, thres)
    allthres <- rbind(allthres, stnzz)
  }

  return(allthres)
}
