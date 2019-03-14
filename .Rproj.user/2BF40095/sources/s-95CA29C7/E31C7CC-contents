#' Scoring edges
#'
#' Calculate a total score for the differences in topology.
#'
#' @param scores_df A scores data frame as calculated by the \code{\link{score_changes}} function.
#' @param test_df A difference test data frame calculated by the \code{\link{difference_test}} function.
#' @return A `tibble`, ranked by total score of the edges
edge_score <- function(scores_df, test_df) {
  if(missing(scores_df)) stop("Need difference scores data frame.")
  if(missing(test_df)) stop("Need test statistic scores.")

  res <- tibble::tibble(x = scores_df$x,
                        y = scores_df$y,
                        cor1 = test_df$cor1,
                        cor2 = test_df$cor2,
                        edge_score = scores_df$score,
                        p_val = test_df$diff_p)

  res <- res %>%
    dplyr::mutate(., p_val = replace(p_val, p_val == 0, min(p_val[pval != 0]) / 10)) %>%
    dplyr::mutate(., log10_p = -log10(p_val)) %>%
    dplyr::mutate(., total_score = abs(cor1) + abs(cor2) + edge_score + log10_p) %>%
    dplyr::filter(., edge_score != 0) %>%
    dplyr::arrange(., desc(total_score))

  return(res)
}
