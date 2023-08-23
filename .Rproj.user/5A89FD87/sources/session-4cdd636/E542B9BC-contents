#' Fit Extreme Distributions (GEV and Gumbel) to Peak Flow Data
#'
#' @description This function fits the Generalized Extreme Value (GEV) and Gumbel distributions to peak flow data from a given station.
#'
#' @param Stn A dataframe containing the peak flow data in a column named 'peak_va'.
#'
#' @importFrom extRemes fevd
#'
#' @return A dataframe with fitted parameters for the GEV and Gumbel distributions.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe 'Stn' with peak flow data:
#' results <- fitExtremeDistributions(Stn)
#' }
#'
fitExtremeDistributions <- function(Stn) {
  # Ensure the required package is loaded
  #require(extRemes)

  # Extracting peak flow data
  PeakF <- Stn$peak_va

  # Empirical Distribution
  X <- sort(PeakF, decreasing = FALSE)

  # Fit the generalized extreme value (GEV) distribution
  fit1 <- fevd(X, type = "GEV")
  plot(fit1)

  # Fit the Gumbel distribution
  fit2 <- fevd(X, type = "Gumbel")
  plot(fit2)

  # Extract parameters
  loc1 <- as.numeric(fit1$results$par[1])
  scl1 <- as.numeric(fit1$results$par[2])
  shp1 <- as.numeric(fit1$results$par[3])

  loc2 <- as.numeric(fit2$results$par[1])
  scl2 <- as.numeric(fit2$results$par[2])

  # Create result data frames
  res_GEV <- data.frame(Distribution = "GEV", Location = loc1, Scale = scl1, Shape = shp1)
  res_Gum <- data.frame(Distribution = "Gumbel", Location = loc2, Scale = scl2, Shape = NA)

  # Combine results
  test_result2 <- rbind(res_GEV, res_Gum)

  return(test_result2)
}
