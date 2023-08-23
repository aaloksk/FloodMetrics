#Calling necessary libraries
library(devtools)
library(usethis)

#1. Plot Annual Peakflow
usethis::use_r("plotAnnualPeakFlow")

#2. FlowData Trend, ChangePoint and Stationarity
usethis::use_r("flowDataDiagnostics")

#3. Acf and Pacf
usethis::use_r("plotAutocorrelations")

#4. Fit Distributions and Test Goodness of fit
usethis::use_r("fitDistTestGOF")

#5. Fit GEV and Gumbel
usethis::use_r("fitExtremeDistributions")

#6. Compare GEV and the best fit
usethis::use_r("compareDistributions")


#2.1 Function for baseflow separation
usethis::use_r("separateBaseflowAndFloodflow")

#2.2 Function to extract Peak Flow and Time to Peak
usethis::use_r("extractPeakFlow")

#2.3 Independent Check
usethis::use_r("independenceCriteriaCheck")

#2.4 Analysie the metrics of flood (Annualization of Daily FLow)
usethis::use_r("analyzeFloodMetrics")

#2.4.1 Generalized Pareto Distribution for Flood Metrics
usethis::use_r("GenParPois")

#2.4.2 Calculate Flood Metrics for Various Thresholds
usethis::use_r("RP_thres")

#2.5 Compute GEV parameters from GP
usethis::use_r("GEVPars")


#3.1 Generate Flood Characterstics
usethis::use_r("floodCharacteristics")

#3.2 Bivariate Copula Analysis with Best Fit Distributions
usethis::use_r("bivariateCopulaWithBestFit")









usethis::use_r("extractFitStatistics")

usethis::use_r("fitMultipleDistributions")

usethis::use_r("fitExpAndGammaDistributions")

usethis::use_r("gringorten2DBivariateEmpirical")

usethis::use_r("cumulativeDensityFunction")

usethis::use_r("fitAndTestDistributions")








#usethis::use_description()


setwd("C:/Users/Aalok/OneDrive - lamar.edu/0000000000Research/Flooding_Risk/R_Package")

devtools::document("FloodMetrics")

devtools::check("FloodMetrics")

#Load all functions in the package
devtools::load_all("FloodMetrics")

devtools::build_manual("FloodMetrics")

library(FloodMetrics)
