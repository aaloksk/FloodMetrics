#' Generalized Pareto Distribution for Flood Metrics
#'
#' @description This function fits a Generalized Pareto Distribution to flood flow data.
#'
#' @param i An identifier or index.
#' @param FldFl A dataframe containing the flood flow data.
#' @param Min_AP Minimum Annual Precipitation for Annual Maximum Series.
#' @param thres The threshold for the Generalized Pareto Distribution.
#'
#' @importFrom fitdistrplus fitdist
#' @importFrom lfstat gringorten
#' @importFrom extRemes fevd
#' @importFrom nortest ad.test
#' @importFrom lubridate year
#' @importFrom eva gpdAd pgpd
#'
#' @return A dataframe with parameters and statistics related to the fitted Generalized Pareto Distribution.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe 'FldFl' with flood flow data, an identifier 'i', Min_AP, and threshold 'thres':
#' result_df <- GenParPois(i, FldFl, Min_AP, thres)
#' }
#'
GenParPois <- function(i, FldFl, Min_AP, thres){
  #Extracting data to fit Generalized Pareto Distribution
  X <- FldFl$Qzpeak
  X <- sort(X, decreasing=FALSE) #Arranging the data in ascending order

  #Threshold Cutoff
  X <- X[X>Min_AP]

  #Getting unique
  X <- unique(X)

  #Threshold about AM
  new_min <- quantile(X, probs=thres)
  X <- X[X>new_min]
  lenX <- length(X)

  #Fitting empirical distibution
  Fit.emp <- gringorten(X)

  #Emperical distribution
  #library(lmomco)
  #Fit.emp2 <- pp(X, a=0.44) #Gringorten #a=0.44 is Optimized for Gumbel distribution.

  #Fitting Generalized Pareto
  gpdfit <- fevd(X, threshold = Min_AP, type="GP", method="Lmoments")

  gpdfit2 <- fevd(X, threshold = Min_AP, type="GP")
  look1 <- summary(gpdfit2)
  AIC <- c(look1$AIC)

  #Extracting parameter
  shp <- as.numeric(gpdfit$results[2])
  scl <- as.numeric(gpdfit$results[1])

  #Getting probabilities
  Fit.theo <- pgpd(X, shape = shp, scale = scl, loc=new_min)
  Z <- unique(X)
  ad_res <- gpdAd(Z)

  ks_i <- ks.test(Fit.emp,Fit.theo)
  ks_i

  ##############################################################################################
  #Extracting year only

  FldFl2 <- FldFl[FldFl$Qzpeak>new_min,]
  yr <- year(FldFl2$Tzpeak)

  #Getting number of peaks for each year
  PPY <- as.data.frame(table(yr))
  Y <- sort(PPY$Freq, decreasing=FALSE) #Getting the data in ascending order
  lenY <- length(Y) #Number of elements in Y

  #Fitting empirical distibution
  Fit2.emp <- gringorten(Y)

  #Fitting Poisson Distribution in PPY data
  poisfit <- fitdist(Y,'pois',method='mle')

  #Get lamda parameter of poisson fit
  lam <- as.numeric(poisfit$estimate[1])

  #Get the cdf using the lamda par
  Fit2.theo <- ppois(Y, lambda = lam)

  #plot(Fit2.emp,Fit2.theo)
  #abline(a=0, b=1)

  #Calculation of dispersion index
  PPY$num <- (PPY$Freq-lam)**2 #Numerator
  dis_ind <- sum(PPY$num)/sum(PPY$Freq)
  dis_ind

  zz <- data.frame(lenX,new_min, shp,scl,new_min,AIC, ks_i$statistic, ks_i$p.value, lam, dis_ind,ad_res$statistic, ad_res$p.value)
  colnames(zz) <- c('NoOfFlood','Thres_cfs', 'GP_shape', 'GP_scale', 'GP_Location','AIC', 'KS_Statistic', 'KS_Pvalue', 'Pois_Lam', 'Dis_Ind','AD_stat', 'AD-pval')

  return (zz)
}
