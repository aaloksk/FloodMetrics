#' Independence Criteria Check on Flood
#'
#' @description This function applies independence criteria checks on flood data and filters out flows below a given threshold.
#'
#' @param QT A dataframe with peak flows and corresponding time obtained after baseflow separation.
#' @param Ar Area of the watershed in square kilometers.
#' @param daily_flow Dataframe with daily flow data.
#' @param flow_threshold The value to filter out flows (default is set to 10).
#'
#' @return A dataframe with peak flows and their corresponding dates after applying the independence criteria checks.
#' @export
#'
#' @examples
#' \dontrun{
#' #Assuming you have dataframes 'QT' with peak flows, 'daily_flow' with daily flow data, and area 'Ar':
#' result_df <- independenceCriteriaCheck(QT, Ar, daily_flow)
#' }
#'
independenceCriteriaCheck <- function(QT, Ar, daily_flow, flow_threshold = 10) {

  #Independence Criteria Check on Flood
  #Explained on Zhang et. al., 2021
  # Automatic procedure for selecting flood events and identifying flood characteristics from daily streamflow data

  #Input Data on this code
  #1. Dataframe with peak flows and corresponding time obtained after baseflow separation
  #2. Area of the watershed in square kilometers



  #Function that takes a dataframe with Time and Peak flow as df_QT
  #Ar must be the are of the catchment in square kilometers
  Criteria1 <- function(df_QT, Ar) {

    #RHS of Independence Criteria 1
    rhs <- 5 + log(Ar/(1.609**2))

    #Row for Criteria Indicator
    df_QT$Cr1 <- 0

    #First flood is always good
    df_QT$Cr1[1] <- 1

    k <-2 #sTARTING A LOOP FROM SECOND ROW
    n_fld <- nrow(df_QT) #Number of flood events

    #Calculating theta
    #Interval between two consecutive peak days
    while(k<=n_fld){
      date1 <- df_QT$Tzpeak[k-1] #First Peak date
      date2 <- df_QT$Tzpeak[k] #Following consecutive peak date
      ndays <- as.numeric(difftime(date2,date1, units = "days")) #Interval between peaks

      if (ndays>rhs){df_QT$Cr1[k]<-1} #COmparing with RHS and changing indicator if needed
      k <- k+1 #FOr next iteration
    }

    #Separating out independent floods
    df_QT_new <- df_QT[df_QT$Cr1 == 1,]

    #Returning intioendent flood event
    return(df_QT_new)
  }



  #Function that performs Independence Check Criteria 2
  Criteria2 <- function(StnNo, QT_cr1, daily_flow){
    #Dataframe with peakflows and corresponding time which has passed criteria 1
    dfQ_cr1 <- QT_cr1
    head(dfQ_cr1)

    dfQ_cr1$Cr2 <- 0


    dfQ_cr1$Cr2[1] <- 1

    n_days <- nrow(daily_flow) #Number of days n daily flow data
    n_fld <- nrow(dfQ_cr1)     #Number of flood events

    evn <- 2

    while(evn<=n_fld){
      tev1 <- dfQ_cr1$Tzpeak[evn-1]
      tev2 <- dfQ_cr1$Tzpeak[evn]

      #Minimum of two floods ###USE AND STATEMENT
      Qmin <- min(dfQ_cr1$Qzpeak[evn-1],dfQ_cr1$Qzpeak[evn])
      Qmin34 <- (3/4) * Qmin

      int_flow_df <- daily_flow[(daily_flow$Date > tev1) & (daily_flow$Date < tev2), ]
      int_flow_Q <- int_flow_df$FloodQ
      min_intQ <- min(int_flow_Q)

      if(min_intQ<Qmin34){dfQ_cr1$Cr2[evn]<-1}
      evn <- evn+1

    }

    dfQ_cr2 <- dfQ_cr1

    return(dfQ_cr2)
  }

  # Applying Criteria 1 multiple times
  QT_1 <- Criteria1(QT, Ar)
  QT_2 <- Criteria1(QT_1, Ar)
  QT_3 <- Criteria1(QT_2, Ar)

  # Applying Criteria 2
  QT_4 <- Criteria2(1, QT_3, daily_flow)

  # Removing flows below the threshold
  QT_5 <- QT_4[QT_4$Qzpeak > flow_threshold, ]

  return(QT_5)
}
