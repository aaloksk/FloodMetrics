#' Fit Multiple Distributions and Test Goodness of Fit
#'
#' @description This function fits multiple distributions to peak flow data, computes empirical probabilities, and tests the goodness of fit using the KS and AD tests.
#'
#' @param Stn A dataframe containing the peak flow data in a column named 'peak_va'.
#' @param StnID A numeric or character value representing the station ID.
#'
#' @import fitdistrplus
#' @importFrom lfstat gringorten
#' @import nortest
#'
#' @return A dataframe containing fit parameters and test statistics for each distribution.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe 'Stn' and a station ID '08041500':
#' fitAndTestDistributions(Stn, '08041500')
#' }
#'
fitAndTestDistributions <- function(Stn, StnID) {

  # Extracting peak flow data
  PeakF <- Stn$peak_va

  # Empirical Distribution
  X <- sort(PeakF, decreasing = FALSE)
  PeakF.emp <- lfstat::gringorten(X)

  # Dataframe for fitted metrics
  fit_params <- data.frame()
  adt_df <- data.frame()

  # Fitting distributions using the fitMultipleDistributions function
  fit <- fitMultipleDistributions(X)

  # Adding StnID to fit dataframe
  fit$StnID <- StnID

  # Number of distributions fitted
  n_fits <- nrow(fit)
  ks_df <- data.frame()

  for(j in seq(1, n_fits, 1)) {
    PeakF.theo <- cumulativeDensityFunction(fit$Dist_name[j], X, fit$shape[j], fit$scale[j])

    ks_i <- ks.test(PeakF.emp, PeakF.theo)
    ks_stat <- ks_i$statistic
    ks_pval <- ks_i$p.value

    ks_params <- c(ks_stat, ks_pval)
    ks_df <- rbind(ks_df, ks_params)
    colnames(ks_df) <- c('KS_stat', 'KS_pval')

    adt_i <- ad.test(PeakF.theo)
    adt_df <- rbind(adt_df, adt_i)
  }

  fit$KS_stat <- ks_df$KS_stat
  fit$KS_pval <- ks_df$KS_pval

  # Appending all results
  fit_params <- rbind(fit_params, fit)

  return(fit_params)
}
