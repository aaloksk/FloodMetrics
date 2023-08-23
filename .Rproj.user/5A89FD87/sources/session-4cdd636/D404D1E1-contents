#Individually checking various functions of the library using the actual data

#Calling necessary libraries
library(devtools)
library(usethis)



setwd("C:/Users/Aalok/OneDrive - lamar.edu/0000000000Research/Flooding_Risk/R_Package")

install("FloodMetrics")

library(FloodMetrics)

#Setting working directory
path <- 'C:\\Users\\Aalok\\OneDrive - lamar.edu\\0000000000Research\\Flooding_Risk\\Station1'
setwd(path)


#Importing peak Streamflow
#Must have a column labeled peak_va
AnPkFl <- read.csv('Stn1_08041500.csv')  ########################################################################
start_yr <- 1940                         ########################################################################
end_yr <- 2022                           ########################################################################
Ar <- 2228.481                           ########################################################################


#Plot Annual Peak Data
plotAnnualPeakFlow(AnPkFl, start_yr, end_yr)

#All trend, stationarity and change point tests
results <- flowDataDiagnostics(AnPkFl$peak_va)

#Acf and Pacf Plots
plotAutocorrelations(AnPkFl$peak_va)


#Test Statistics
library (actuar)
test_result <- fitDistTestGOF(AnPkFl, '08041500')
test_result2 <- fitExtremeDistributions(AnPkFl)


#Comparing the best fit and GEV

# Define properties for sample1          ########################################################################
sample1_properties <- list(
  distribution = "lnorm",
  meanlog = 9.184,
  sdlog = 0.975997
)

# Define properties for sample2          ########################################################################
sample2_properties <- list(
  distribution = "gev",
  loc = 7106.564,
  shape = 0.605009,
  scale = 6221.046
)

# Define the percentiles (if different from the default)
p <- c(0.05,seq(0.1,0.9,0.005), 0.95)


library(evd)
# Call the function
result_comparision <- compareDistributions(sample1_properties, sample2_properties, p, "CDF_BF_GEV.svg")

# Print the result
print(result_comparision)






#Daily flow
DlFl <- read.csv('Stn1DF.csv')          ########################################################################


#SeparateBaseFlowandPeakFlow
DlPF <- separateBaseflowAndFloodflow(DlFl, start = "10/1/1939", end = "9/30/2022")

#ExtractPeakFlow
DL_PeakQT <- extractPeakFlow(DlPF)


DL_IndCk <- independenceCriteriaCheck(DL_PeakQT, Ar, DlFl)





#Output is Stn1_FilFLood in FldFl
FldFl <- read.csv('Stn1_FilFlood.csv')
i <- 1
percentle <- 0.75

# Calculate thres_cfs using the percentile
Thres_cfs <- quantile(FldFl$Qzpeak, percentle)

#Minimum Annual Precipitation for Annual Maximum Series.
Min_AP <- 397

#Analyzing the flood metrics with Generalized Pareto and Poisson to annualize POT
result_allthres <- analyzeFloodMetrics(FldFl, i, Min_AP, percentle)

#Generalized Extreme Value
result_GEVpars <- GEVPars(result_allthres)
result_GEVpars



#BIVARIATE
#Getting Floods Characteristics
floodq <- read.csv('Stn1_FloodQ.csv')
zz <- floodCharacteristics(floodq, '08041500')

library (actuar)
bivariateCopulaWithBestFit(zz$Volume, zz$Duration)
