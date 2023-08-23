#' Fit Multiple Distributions to Data
#'
#' @description This function fits five candidate distributions to the provided data using Maximum Likelihood Estimation (MLE).
#' The distributions fitted are: normal, lognormal, weibull, gamma, and logistic.
#'
#' @param X A numeric vector of data to be fitted into various distributions.
#' @import fitdistrplus
#' @import extRemes
#' @import actuar
#'
#' @return A data frame containing the fit statistics for each distribution, including their shape, scale,
#' Log-likelihood, and AIC value.
#' @export
#'
#' @examples
#' \dontrun{
#' fitMultipleDistributions(data)
#' }
#'
fitMultipleDistributions <- function(X) {
  zz <- data.frame()

  # Fit Normal Distribution
  X.dist <- fitdist(X, 'norm', method = 'mle')
  X.met <- extractFitStatistics(X.dist)
  zz <- rbind(zz, X.met)

  # Fit Lognormal Distribution
  X.dist <- fitdist(X, 'lnorm', method = 'mle')
  X.met <- extractFitStatistics(X.dist)
  zz <- rbind(zz, X.met)

  # Fit Weibull Distribution
  X.dist <- fitdist(X, 'weibull', method = 'mle')
  X.met <- extractFitStatistics(X.dist)
  zz <- rbind(zz, X.met)

  # Fit Gamma Distribution
  X.dist <- fitdist(X, 'gamma', method = 'mle', lower = c(0, 0))
  X.met <- extractFitStatistics(X.dist)
  zz <- rbind(zz, X.met)

  # Fit Logistic Distribution
  X.dist <- fitdist(X, 'llogis', method = 'mle')
  X.met <- extractFitStatistics(X.dist)
  zz <- rbind(zz, X.met)

  # Set column names for the result
  colnames(zz) <- c('Dist_name', 'shape', 'scale', 'SEshape', 'SEscale', 'Loglik', 'AIC')

  return(zz)
}
