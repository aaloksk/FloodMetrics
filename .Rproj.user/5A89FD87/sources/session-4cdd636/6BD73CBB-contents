#' Bivariate Copula Analysis with Best Fit Distributions
#'
#' This function fits two datasets to multiple distributions, selects the best distribution based on AIC,
#' and then performs a bivariate copula analysis.
#'
#' @param X1 A numeric vector representing the first data set.
#' @param X2 A numeric vector representing the second data set.
#'
#' @import VineCopula
#' @importFrom graphics contour plot abline
#' @import actuar
#' @import stats
#'
#' @return A list containing the results of the copula analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have two vectors 'X1' and 'X2' and a function 'fitMultipleDistributions':
#' result <- bivariateCopulaWithBestFit(X1, X2, fitMultipleDistributions)
#' }
#'
#'
bivariateCopulaWithBestFit <- function(X1, X2) {

  gringorten2D <- function(idx,X){
    dat <- X
    val <- X[idx,]
    kk <- ifelse((dat[,1]<=val[1] & dat[,2]<=val[2]),1,0)
    zz <- (sum(kk) - 0.12)/(nrow(X)+0.44)
    return(zz)
  }

  # Fit X1 and X2
  fit_X1 <- fitMultipleDistributions(X1)
  fit_X2 <- fitMultipleDistributions(X2)

  # Select Best fit for X1
  min_index_X1 <- which.min(fit_X1$AIC)
  best_X1 <- fit_X1[min_index_X1, ]

  # Select Best fit for X2
  min_index_X2 <- which.min(fit_X2$AIC)
  best_X2 <- fit_X2[min_index_X2, ]

  # Get CDF values based on best fit
  pX1 <- cumulativeDensityFunction(best_X1$Dist_name, X1, best_X1$shape, best_X1$scale)
  pX2 <- cumulativeDensityFunction(best_X2$Dist_name, X2, best_X2$shape, best_X2$scale)

  # Joint Probability using 2D Gringorten
  X <- cbind(X1, X2)
  idx <- 1:nrow(X)
  FX <- sapply(idx, gringorten2D, X)
  FXX <- cbind(X, FX)

  # Copula
  aa <- BiCopSelect(pX1, pX2, familyset = c(1:9), rotations = TRUE)
  jprob <- BiCopCDF(pX1, pX2, family = aa$family, par = aa$par, par2 = aa$par2)
  az <- cbind(FXX, jprob)
  colnames(az) <- c('X1', 'X2', 'Empirical', 'Theoretical')

  # PP Plot
  plot(FX, jprob, xlab = 'Empirical Probability', ylab = 'Theoretical Probability', main = "Bivariate Analysis")
  abline(coef = c(0, 1), col = 'blue', lw = 2)

  # Storing copula information
  zz <- data.frame(aa$familyname, aa$par, aa$par2, aa$tau, aa$logLik, aa$AIC)
  colnames(zz) <- c('FamilyName', 'Parameter', 'Parameter2', 'Tau', 'LogLik', 'AIC')

  # Create sequence of two variables
  seqX1 <- seq(min(X1), max(X1), length.out = 201)
  seqX2 <- seq(min(X2), max(X2), length.out = 201)

  pSeqX1 <- cumulativeDensityFunction(best_X1$Dist_name, seqX1, best_X1$shape, best_X1$scale)
  pSeqX2 <- cumulativeDensityFunction(best_X2$Dist_name, seqX2, best_X2$shape, best_X2$scale)

  pars <- expand.grid(pSeqX1, pSeqX2)
  jprob_grid <- BiCopCDF(pars[, 1], pars[, 2], family = aa$family, par = aa$par, par2 = aa$par2)
  jpmat <- matrix(jprob_grid, 201, 201, byrow = FALSE)

  contour(seqX1, seqX2, jpmat, levels = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95),
          xlab = 'X1', ylab = 'X2', col = 'blue')

  return(list(az = az, copula_info = zz))
}
