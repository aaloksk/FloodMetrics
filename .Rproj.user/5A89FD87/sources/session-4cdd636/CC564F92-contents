#' Compute GEV Parameters
#'
#' This function calculates the GEV parameters based on the provided dataframe.
#'
#' @param a A dataframe containing the columns 'GP_Location', 'GP_scale', 'GP_shape', and 'Pois_Lam'.
#'
#' @return A dataframe with added columns for 'GEV_loc', 'GEV_scale', and 'GEV_shp'.
#'
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a dataframe 'FldFl' with flood flow data, an identifier 'i', Min_AP, and percentile:
#' result_df <- GEVPars(df)
#' }
#'
GEVPars <- function(a){

  # Calculate GEV location parameter
  a$GEV_loc <- a$GP_Location + (a$GP_scale/a$GP_shape) * (1 - ((a$Pois_Lam)**((-1) * a$GP_shape)))

  # Calculate GEV scale parameter
  a$GEV_scale <- a$GP_scale * ((a$Pois_Lam)**((-1) * a$GP_shape))

  # Assign GEV shape parameter
  a$GEV_shp <- a$GP_shape

  return(a)
}
