#' Fit Statistics Extraction
#' @description Function used to obtain coefficients and Metrics for each distribution
#' @description Takes the object fitted using fitdistrplus
#' @description Extracts - Shape, Scale, SDShape, SDscale, Loglikelihood and AIC
#'
#' @param obj Object that is obtained after fitting a set of data to fitdist function offitdistplus library
#' @import fitdistrplus
#' @import extRemes
#' @import actuar
#'
#' @return Shape, Scale, SDShape, SDscale, Loglikelihood and AIC of the distribution
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate some sample data
#' data <- rnorm(100)
#'
#' # Fit the data using fitdist from the fitdistrplus package
#' fit <- fitdistrplus::fitdist(data, "norm")
#'
#' # Extract fit statistics
#' extractFitStatistics(fit)
#' }
#' #'
extractFitStatistics <- function(obj) {
  dist_name <- obj$distname
  estimates <- as.vector(obj$estimate) # coefficients
  std_errors <- as.vector(obj$sd) # standard errors
  log_likelihood <- as.vector(obj$loglik) # log-likelihood
  aic_value <- as.vector(obj$aic)  # AIC value

  result <- data.frame(
    Distribution = dist_name,
    Shape = estimates[1],
    Scale = estimates[2],
    SDShape = std_errors[1],
    SDScale = std_errors[2],
    LogLikelihood = log_likelihood,
    AIC = aic_value
  )

  return(result)
}
