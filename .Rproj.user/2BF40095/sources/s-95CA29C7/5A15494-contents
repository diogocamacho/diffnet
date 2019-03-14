#' diffnet
#'
#' Wrapper for the package.
#'
#' @param data Data matrix, where rows are observations (eg, genes) and columns are samples.
#' @param group1 Column ids for group 1.
#' @param group2 Column ids for group 2.
#' @param correlation_type Method to calculate correlation.  Defaults to Spearman correlations.
#' @param correlation_thr Correlation threshold for edge assignment.  Defaults to 0.75
#' @return Returns a `tibble` that is sorted by the total edge score.
diffnet <- function(data,
                    group1,
                    group2,
                    correlation_type = NULL,
                    correlation_thr)
{

  # function argument checks ----
  if(missing(correlation_type)) correlation_type <- "spearman"
  if(missing(correlation_thr)) correlation_thr <- 0.75
  if(missing(group1) | missing(group2)) stop("Need two groups.")

  message("--- DiffNet: Analyzing topological differences of networks ---")

  # compute correlations ----
  message("Calculating correlations...")
  cors <- calculate_correlations(data = data,
                                 group1 = group1,
                                 group2 = group2,
                                 method = correlation_type)

  # clean up correlations ----
  message("Cleaning correlation matrices...")
  clean_cor <- cleanup(correlation_df = cors)

  # define interactions ----
  message("Defining interactions...")
  interaction_df <- define_interactions(correlation_df = clean_cor,
                                        correlation_thr = correlation_thr)

  # correlation difference test ----
  message("Correlation difference test...")
  diff_test <- difference_test(correlation_df = interaction_df,
                               g1_size = length(group1),
                               g2_size = length(group2))

  # score topology changes ----
  message("Calculate total edge scores...")
  diff_scores <- score_changes(interaction_df = interaction_df)

  # edge scores ----
  message("Writing results data frame...")
  res <- edge_score(scores_df = diff_scores, test_df = diff_test)

  message("DONE.")

  return(res)


}
