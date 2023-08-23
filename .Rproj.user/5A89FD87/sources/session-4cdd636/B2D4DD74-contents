#' Analyze Flow Data for Trends, Change Points, and Stationarity
#'
#' @description This function conducts a series of tests on flow data to detect trends, change points, and assess stationarity. The tests performed include the Mann-Kendall Test, Pettitt Test, Augmented Dickey-Fuller Test, KPSS Test, and the Phillips-Perron Unit Root Test.
#'
#' @param pf A numeric vector or time series representing flow data.
#'
#' @importFrom Kendall MannKendall
#' @importFrom trend pettitt.test
#' @importFrom tseries adf.test
#' @importFrom tseries kpss.test pp.test
#'
#' @return A list containing the results of each of the tests performed.
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- rnorm(100)  # Example data
#'   results <- flowDataDiagnostics(data)
#'   print(results)
#' }
#'
#' @references McLeod, A. I. (2005). Kendall rank correlation and Mann-Kendall trend test. R Package Kendall, 602, 1-10.
#'

flowDataDiagnostics <- function (pf){

  # Load necessary libraries for the tests
  #library('Kendall')    # For Mann-Kendall Test
  #library('trend')      # For Pettitt Test
  #library('forecast')   # For ACF test
  #library('tseries')    # For ADF test

  # Perform Mann Kendall Test for trend detection
  # Reference: https://www.statology.org/mann-kendall-trend-test-r/
  mk_res <- MannKendall(pf)
  # The summary of the test can be viewed using summary(mk_res)
  # The test provides Tau (strength of trend) and p-value (statistical significance of the trend)

  # Perform Pettitt Test for change point detection
  pet_res <- pettitt.test(pf)
  # The result of the test can be viewed using pet_res

  # Perform Augmented Dickey Fuller Test (ADF) for stationarity
  # Reference: https://www.r-bloggers.com/2022/06/augmented-dickey-fuller-test-in-r/
  adf_res <- adf.test(pf)
  # The result of the test can be viewed using adf_res

  # Perform KPSS Test for stationarity with trend as null hypothesis
  # Reference: https://www.statology.org/kpss-test-in-r/
  kpss_res <- kpss.test(pf, null = 'Trend')

  # Perform Phillips-Perron Unit Root Test for stationarity
  # Two variations of the test are performed: with short and long lags
  pp_res1 <- pp.test(pf, lshort = TRUE)
  pp_res2 <- pp.test(pf, lshort = FALSE)

  # Combine all test results into a list and return
  aa_res <- list (mk_res, pet_res, adf_res, kpss_res, pp_res1, pp_res2)
  return(aa_res)
}
