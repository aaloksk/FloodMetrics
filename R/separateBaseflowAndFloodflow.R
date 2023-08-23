#' Separate Streamflow into Baseflow and Floodflow
#'
#' @description This function infills missing data using linear interpolation and separates baseflow from daily streamflow data.
#'
#' @param a A dataframe containing the streamflow data.
#' @param start The start date for the station in the format "mm/dd/yyyy".
#' @param end The end date for the station in the format "mm/dd/yyyy".
#'
#' @importFrom zoo na.approx
#'
#' @return A dataframe with infilled data and separated baseflow and floodflow.
#' @export
#'
#' @details The BFI function is sourced from an external URL:
#' \url{https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R}
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe 'a' with streamflow data and start and end dates:
#' result_df <- separateBaseflowAndFloodflow(a, start = "10/1/1939", end = "9/30/2022")
#' }
#'
separateBaseflowAndFloodflow <- function(a, start, end) {
  # source the BFI function
  source('https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R')

  a$Dis_cfs <- as.numeric(a$Dis_cfs)

  # Create the sequence of all dates
  bgn <- as.Date(start, format = "%m/%d/%Y")
  end <- as.Date(end, format = "%m/%d/%Y")
  datea <- seq.Date(bgn, end, by = 'day')

  # Arranging the data to a new dataframe with created date
  df <- data.frame(datea, a$Dis_cfs)
  colnames(df) <- c('Date', 'MeanQ')

  # Filling NA by linear interpolation
  df$MeanQ <- na.approx(df$MeanQ)

  # Extracting streamflow to a new variable
  Q <- df$MeanQ

  # Calling BFI function with 0.9 as alpha
  Qbase <- BFI(Q, a = 0.9, ReturnQbase = TRUE, n.reflect = 30)$Qbase

  # Arranging base flow and floodflow into the dataframe
  df$BaseQ <- Qbase
  df$FloodQ <- floor((df$MeanQ - df$BaseQ)) # Round down
  df$Floodidx <- ifelse(df$FloodQ == 0, 0, 1)

  return(df)
}
