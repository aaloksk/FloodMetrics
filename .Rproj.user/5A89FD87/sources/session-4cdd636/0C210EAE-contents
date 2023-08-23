#' Time Series Plot of Annual Peak Flow
#' @description This function takes annual peak flow data and plots it as a time series for a specified range of years.
#' @param df A dataframe with annual peak flow in a column labeled peak_va.
#' @param st_yr Start Year.
#' @param end_yr End Year.
#'
#' @importFrom graphics grid
#'
#' @return Time Series Plot
#' @export
#'
#' @examples
#' \dontrun{
#' plotAnnualPeakFlow(df,1980,2022)
#' }
#'
plotAnnualPeakFlow <- function(df, st_yr, end_yr) {

  # Error Handling
  if (!"peak_va" %in% colnames(df)) {
    stop("The dataframe does not contain a 'peak_va' column.")
  }

  if (st_yr > end_yr) {
    stop("Start year must be less than or equal to the end year.")
  }

  # Convert years to date format for sequence creation
  start_date <- as.Date(paste(st_yr,'-01-01',sep=''))
  end_date <- as.Date(paste(end_yr,'-01-01',sep=''))

  # Create the sequence of all date years for streamflow
  date_seq <- seq.Date(start_date, end_date, by = 'year')

  # Create dataframe for the time series plot
  pf_ts <- data.frame(Date = date_seq, Peak_Flow = df$peak_va)

  # Time series plot of Annual Peak
  plot(pf_ts$Date, pf_ts$Peak_Flow,
       xlab = "Year",
       ylab = "Annual peak streamflow value in cfs",
       pch = 19, col = 4,
       main = "Annual Peak Flow Time Series")
  grid(nx = 41, ny = 5)
}
