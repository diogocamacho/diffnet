#' Define interactions
#'
#' Given a correlation data frame (as calculated by \code{\link{calculate_correlations}}) and a correlation threshold, defines the set of interactions in a correlation adjacency matrix.
#'
#' @param correlation_df A correlation data frame (see \code{\link{calculate_correlations}})
#' @param correlation_thr A threshold for correlations (defaults to 0.75)
#' @return An interaction data frame, where the presence or absence of an edge in the correlation network is given as a 1 or a 0 (respectively)
define_interactions <- function(correlation_df, correlation_thr) {
  if(missing(correlation_df)) stop("Need correlation data frame.")
  if(missing(correlation_thr)) correlation_thr <- 0.75

  int_df <- correlation_df %>%
    tibble::add_column(., n1 = 0, n2 = 0) %>%
    dplyr::mutate(., n1 = replace(n1, abs(cor1) >= correlation_thr, 1))  %>%
    dplyr::mutate(., n2 = replace(n2, abs(cor2) >= correlation_thr, 1))

  return(int_df)
}
