#' Plot Autocorrelation and Partial Autocorrelation
#'
#' @description This function generates the Autocorrelation Function (ACF) and 'Partial Autocorrelation Function (PACF) plots for the given data.
#'
#' @param df A numeric vector or time series.
#'
#' @importFrom forecast Acf Pacf
#'
#' @return Autocorrelation Function (ACF) and Partial Autocorrelation Function (PACF) plots.
#' @export
#'
#' @examples
#' \dontrun{
#' plotAutocorrelations(data)
#' }
#'
plotAutocorrelations <- function(df){
  # Generate the ACF plot
  forecast::Acf(df, main = "")
  # Generate the PACF plot
  forecast::Pacf(df, main = "")
}
