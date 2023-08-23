#' Fit Exponential and Gamma Distributions to Data
#'
#' @description This function fits both exponential and gamma distributions to the provided data using Maximum Likelihood Estimation (MLE).
#'
#' @param X A numeric vector of data to be fitted into the distributions.
#' @import fitdistrplus
#'
#' @return A data frame containing the fit statistics for each distribution, including their shape, scale,
#' standard errors for shape and scale, Log-likelihood, and AIC value.
#' @export
#'
#' @examples
#' \dontrun{
#' fitExpAndGammaDistributions(data)
#' }
#'
fitExpAndGammaDistributions <- function(X) {
  # Ensure the fitdistrplus library is loaded
  #require(fitdistrplus)

  # Fit Exponential Distribution
  exp_fit <- fitdist(X, 'exp', method = 'mle')
  zz1 <- extractFitStatistics(exp_fit)

  # Fit Gamma Distribution
  gam_fit <- fitdist(X, 'gamma', method = 'mle', lower = c(0, 0))
  zz2 <- extractFitStatistics(gam_fit)

  # Combine results
  zz <- rbind(zz1, zz2)
  colnames(zz) <- c('Dist_name', 'shape', 'scale', 'SEshape', 'SEscale', 'Loglik', 'AIC')

  return(zz)
}
