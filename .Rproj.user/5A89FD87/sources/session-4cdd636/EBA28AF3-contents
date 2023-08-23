#' Bivariate Empirical Distribution Using Gringorten's Formula
#'
#' @description This function computes the bivariate empirical distribution value for a given data point using Gringorten's plotting formula.
#'
#' @param idx An integer index indicating which row of the data matrix `X` should be used as the reference point.
#' @param X A numeric matrix or data frame where each row represents a bivariate data point.
#'
#' @return A numeric value representing the bivariate empirical distribution value for the specified data point.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- matrix(rnorm(100), ncol = 2)
#' gringorten2DBivariateEmpirical(10, data)
#' }
#'
gringorten2DBivariateEmpirical <- function(idx, X) {
  # Ensure the input is a matrix or data frame
  if (!is.matrix(X) && !is.data.frame(X)) {
    stop("Input X must be a matrix or data frame.")
  }

  # Ensure idx is a valid index
  if (idx < 1 || idx > nrow(X)) {
    stop("Invalid index specified.")
  }

  dat <- X
  val <- X[idx, ]
  kk <- ifelse((dat[, 1] <= val[1] & dat[, 2] <= val[2]), 1, 0)
  zz <- (sum(kk) - 0.12) / (nrow(X) + 0.44)

  return(zz)
}
