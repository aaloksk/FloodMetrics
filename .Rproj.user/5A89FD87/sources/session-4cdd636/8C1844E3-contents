#' Extract Peak Flow and Time to Peak
#'
#' @description This function extracts the peak flow and the corresponding time to peak from a given dataframe.
#'
#' @param df A dataframe containing the streamflow data with columns 'Floodidx', 'FloodQ', and 'Date'.
#'
#' @return A dataframe with peak flows and their corresponding dates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe 'df' with streamflow data:
#' result_df <- extractPeakFlow(df)
#' }
#'
extractPeakFlow <- function(df) {

  n_days <- nrow(df) # Getting number of days
  PeakQ_T <- data.frame() # Initiating a dataframe to store peak flows and corresponding date
  ind <- 0 # Indicator for flood event
  k <- 1 # Indicator for row number

  # Running a while loop on each day
  while(k <= n_days) {

    # Initiating temporary variable to store peak Q and time for each event
    Qzpeak <- 0
    Tzpeak <- 0

    # Loop that runs along all the 1s between two zeros (i.e. One event of flood)
    # Objective is to extract peak flow for that event and corresponding timestamp.
    while(df$Floodidx[k] == 1) {

      if(df$FloodQ[k] > Qzpeak) {
        ind <- 1 # Indicator that flood event has started
        Tzpeak <- df$Date[k]
        Qzpeak <- df$FloodQ[k]
      }

      k = k + 1 # Adding a day within flooding event
    }

    # Conditional statement to enter immediately after a flood event
    if (ind == 1) {
      PeakQT_k <- data.frame(Tzpeak, Qzpeak)
      PeakQ_T <- rbind(PeakQ_T, PeakQT_k)
      ind <- 0 # Resetting the indicator
    }

    k = k + 1 # Adding a day outside the flood event
  }

  return(PeakQ_T)
}
