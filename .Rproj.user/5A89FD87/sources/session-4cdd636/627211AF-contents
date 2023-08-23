#' Compute Flood Characteristics
#'
#' @description This function computes flood characteristics from a given flood data dataframe.
#'
#' @param df Dataframe with flood data.
#' @param StnID ID of the Station
#' @param volume_threshold The volume threshold to filter out floods (default is set to 4355990).
#'
#' @return A dataframe with flood characteristics.
#' @export
#' @importFrom dplyr mutate lag %>%
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe 'df' with flood data:
#' result_df <- floodCharacteristics(df, '08041500')
#' }
#'
floodCharacteristics <- function(df, StnID, volume_threshold = 4355990) {

  df$StnID <- StnID

  # RLE computation
  rleQ <- rle(df$Floodidx)
  rle_tab <- table(rleQ$values, rleQ$lengths)

  n_days <- nrow(df) # Getting number of days
  Fld_df <- data.frame() # Initiating a dataframe to store peak flows and corresponding date
  ind <- 0 # Indicator for flood event
  k <- 1 # Indicator for row number
  FldID <- 1000000 # Initiating variable to get Flood ID

  # Running a while loop on each day
  while(k <= n_days) {

    ind <- 0 # Indicator for flood event

    # Initiating temporary variable to store peak Q and time for each event
    Qzpeak <- 0
    Tzpeak <- 0
    FD <- 0 # Setting flood duration to zero before each event
    V <- 0 # Setting flood volume to zero before each event
    Vi <- 0

    # Getting flood start date
    if(df$Floodidx[k] == 1) {
      Tstart <- df$Date[k]
    }

    # Loop that runs along all the 1s between two zeros (i.e., One event of flood)
    while(df$Floodidx[k] == 1) {

      if(df$FloodQ[k] > Qzpeak) {
        ind <- 1 # Indicator that flood event has started
        Tzpeak <- df$Date[k]
        Qzpeak <- df$FloodQ[k]
      }

      Vi <- 0.5 * (df$FloodQ[k] - df$FloodQ[k-1]) * 86400
      V <- V + abs(Vi)

      FD <- FD + 1 # Counting flood duration
      k <- k + 1 # Adding a day within flooding event
    }

    # Getting flood end date after the completion of event
    Tend <- df$Date[k-1]

    # Conditional statement to enter immediately after a flood event
    if (ind == 1) {
      Fld_stn <- df$StnID[k-1] # Current station
      PeakQT_k <- data.frame(FldID, Fld_stn, Tstart, Tend, Tzpeak, Qzpeak, FD, V)
      Fld_df <- rbind(Fld_df, PeakQT_k)
      FldID <- FldID + 1
      ind <- 0 # Resetting the indicator
    }

    if(df$Floodidx[k] != 1) {
      k <- k + 1 # Adding a day outside the flood event
    }
  }

  colnames(Fld_df) <- c('FloodID', 'Station_ID', 'Start_Date', 'End_Date', 'TPeak', 'QPeak', 'Duration', 'Volume')

  # Floods with duration greater than 10 days
  Flood_df <- Fld_df[Fld_df$Duration > 10,]
  Flood_df$Tim2Pk <- as.integer(difftime(Flood_df$TPeak, Flood_df$Start_Date, units = "days"))

  # Filter the floods with volume less than the given threshold
  FilteredFloods <- Flood_df[Flood_df$Volume > volume_threshold,]

  # Calculate the difference in time
  FilteredFloods <- FilteredFloods %>%
    mutate(EndShift = lag(End_Date),
           Diff = as.numeric(difftime(as.Date(Start_Date), as.Date(EndShift), units = "days")))

  return(FilteredFloods)
}
