#' Cumulative Density of Specified Distribution
#'
#' @description This function computes the cumulative density value for a given distribution and parameters.
#'
#' @param dbn A character string specifying the distribution. Valid options are "norm", "lnorm", "gamma", "weibull", and "llogis".
#' @param P A numeric value or vector for which the cumulative density is to be computed.
#' @param shp Shape parameter for the distribution.
#' @param sc Scale parameter for the distribution.
#'
#' @importFrom stats pnorm plnorm pgamma pweibull ks.test
#' @import actuar
#'
#' @return A numeric value or vector representing the cumulative density for the specified distribution and parameters.
#' @export
#'
#' @examples
#' \dontrun{
#' cumulativeDensityFunction("norm", 0, 0, 1)
#' cumulativeDensityFunction("gamma", 1, 2, 1)
#' }
#'
cumulativeDensityFunction <- function(dbn, P, shp, sc) {
  if (!is.character(dbn) || !(dbn %in% c("norm", "lnorm", "gamma", "weibull", "llogis"))) {
    stop("Invalid distribution specified.")
  }

  if (dbn == "norm") {
    pP = pnorm(P, mean = shp, sd = sc)
  } else if (dbn == "lnorm") {
    pP = plnorm(P, meanlog = shp, sdlog = sc)
  } else if (dbn == "gamma") {
    pP = pgamma(P, shape = shp, scale = 1/sc)
  } else if (dbn == "weibull") {
    pP = pweibull(P, shape = shp, scale = sc)
  } else if (dbn == "llogis") {
    pP = pllogis(P, shape = shp, scale = sc)
  }

  return(pP)
}
