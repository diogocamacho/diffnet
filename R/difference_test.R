#' Correlation difference test
#'
#' Given a correlation data frame (calculated using \code{\link{calculate_correlations}}) calculate Fisher's correlation test.
#'
#' @param correlation_df A correlation data frame
#' @param g1_size Size of group 1
#' @param g2_size Size of group 2
#' @return A data frame with the test statistic
difference_test <- function(correlation_df, g1_size, g2_size) {

  if(missing(correlation_df)) stop("Need correlation data frame.")
  if(missing(g1_size) | missing(g2_size)) stop("Need to know sizes of groups.")

  difference_df <- correlation_df %>%
    dplyr::mutate(., x1 = 0.5 * log((1 + cor1) / (1 - cor1)), x2 = 0.5 * log((1 + cor2) / (1 - cor2))) %>%
    dplyr::mutate(., df1 = 1 / (g1_size - 3), df2 = 1 / (g2_size - 3)) %>%
    dplyr::mutate(., cor_diff = x1 - x2) %>%
    dplyr::mutate(., z = cor_diff / sqrt(df1 + df2)) %>%
    dplyr::mutate(., diff_p = 2 * pnorm(-abs(z)))

  return(difference_df)
}
# logPval <- -log10(p_val)
# logPval[logPval == Inf] <- ceiling(max(logPval[logPval != Inf]))



