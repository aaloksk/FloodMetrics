#' Compare Best-Fit and GEV Distributions
#'
#' @description This function compares the best-fit and GEV distributions using the KS test and plots their CDFs.
#'
#' @param sample1_properties A named list containing the properties of the first sample (distribution and its parameters).
#' @param sample2_properties A named list containing the properties of the second sample (distribution and its parameters).
#' @param p A numeric vector of percentiles.
#' @param output_filename The filename for the SVG output plot.
#'
#' @return A dataframe containing the KS test statistic and p-value.
#' @export
#'
compareDistributions <- function(sample1_properties, sample2_properties, p = c(0.05,seq(0.1,0.9,0.005), 0.95), output_filename = "CDF_comparison.svg") {

  # Extracting samples based on provided properties
  if(sample1_properties$distribution == "lnorm") {
    sample1 <- qlnorm(p, meanlog = sample1_properties$meanlog, sdlog = sample1_properties$sdlog)
  }
  #I will add all other distributions for sample 1.

  if(sample2_properties$distribution == "gev") {
    sample2 <- qgev(p, loc = sample2_properties$loc, shape = sample2_properties$shape, scale = sample2_properties$scale)
  }


  # Perform the KS test
  res <- ks.test(sample1, sample2)
  dfi <- data.frame(statistic = res$statistic, p.value = res$p.value)

  # Calculate ECDFs of the two samples
  ecdf1 <- ecdf(sample1)
  ecdf2 <- ecdf(sample2)

  # Plot cumulative distribution functions of the two samples
  svg(output_filename, width=6, height=6)
  plot(ecdf1, main="CDF Comparison", xlab="Values", ylab="Probability", col="red")
  lines(ecdf2, col="blue")
  legend("bottomright", c("Sample1", "Sample2"), col=c("red","blue"), lty=1)
  dev.off()

  return(dfi)
}
