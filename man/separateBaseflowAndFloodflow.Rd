% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/separateBaseflowAndFloodflow.R
\name{separateBaseflowAndFloodflow}
\alias{separateBaseflowAndFloodflow}
\title{Separate Streamflow into Baseflow and Floodflow}
\usage{
separateBaseflowAndFloodflow(a, start, end)
}
\arguments{
\item{a}{A dataframe containing the streamflow data.}

\item{start}{The start date for the station in the format "mm/dd/yyyy".}

\item{end}{The end date for the station in the format "mm/dd/yyyy".}
}
\value{
A dataframe with infilled data and separated baseflow and floodflow.
}
\description{
This function infills missing data using linear interpolation and separates baseflow from daily streamflow data.
}
\details{
The BFI function is sourced from an external URL:
\url{https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R}
}
\examples{
\dontrun{
# Assuming you have a dataframe 'a' with streamflow data and start and end dates:
result_df <- separateBaseflowAndFloodflow(a, start = "10/1/1939", end = "9/30/2022")
}

}
