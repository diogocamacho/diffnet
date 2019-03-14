#' Correlation matrices
#'
#' This function computes the correlation matrices for two groups given a data set (e.g., an expression matrix.) This function uses the `corrr` package to compute the correlations.
#'
#' @param data The data matrix, with samples on columns and observations on rows
#' @param group1 column ids for group 1
#' @param group2 column ids for group 2
#' @param method correlation type (defaults to Spearman correlations.)
#' @return returns a `tibble` with the correlations between all the vartiables for both groups. x and y in data frame are row ids that can be easily mapped to variable names in data set.
calculate_correlations <- function(data, group1, group2, method) {

  if (missing(data)) stop("Need data.")
  if (missing(group1) | missing(group2)) stop("Need 2 groups to calculate correlation difference.")
  if (missing(method)) method <- "spearman"

  # remove rownames of data matrix ahead of calculation of correlation
  rownames(data) <- NULL
  rownames(data) <- seq(1, nrow(data))

  # calculate correlations
  r1 <- corrr::correlate(t(data[, group1]), method = method, quiet = TRUE) %>%
    corrr::shave(upper = TRUE) %>%
    corrr::stretch()
  r2 <- corrr::correlate(t(data[, group2]), method = method, quiet = TRUE) %>%
    corrr::shave(upper = TRUE) %>%
    corrr::stretch()

  correlations <- tibble(x = as.numeric(r1$x),
                         y = as.numeric(r1$y),
                         cor1 = r1$r,
                         cor2 = r2$r)

  return(correlations)
}
