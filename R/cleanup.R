#' cleanup
#'
#' Function eliminates NAs from matrix and sets absolute value of correlations smaller than user defined correlation threshold to 0.
#'
#' @param correlation_df A corrrelation data frame as calculated with \code{\link{calculate_correlations}}
#' @return A cleaned up correlation matrix
cleanup <- function(correlation_df) {
  if(missing(correlation_df)) stop("Need correlation data frame.")

  # clean NAs
  nix <- intersect(which(is.na(correlation_df$cor1)), which(is.na(correlation_df$cor2)))
  correlation_df <- correlation_df[-nix, ]
  return(correlation_df)
}
