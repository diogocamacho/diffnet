#' Score topology changes
#'
#' Given an interaction data frame (calculated with \code{\link(define_interactions}), provides an edge score and an edge class given the correlation value and the edge presence call.
#'
#' @param interaction_df Interaction data frame
#' @return A score data frame for each edge in the network
score_changes <- function(interaction_df) {

  score_df <- interaction_df %>%
    dplyr::mutate(., score = 0, definition = NA) %>%
    dplyr::mutate(., score = replace(score, n1 == 1 & n2 == 1 & cor1 > 0 & cor2 > 0, 1),
                  definition = replace(definition, n1 == 1 & n2 == 1 & cor1 > 0 & cor2 > 0, "edge present, same sign")) %>%
    dplyr::mutate(., score = replace(score, n1 == 1 & n2 == 1 & cor1 < 0 & cor2 < 0, 1),
                  definition = replace(definition, n1 == 1 & n2 == 1 & cor1 < 0 & cor2 < 0, "edge present, same sign")) %>%
    dplyr::mutate(., score = replace(score, n1 == 1 & n2 == 0, 2),
                  definition = replace(definition, n1 == 1 & n2 == 0, "loss of edge")) %>%
    dplyr::mutate(., score = replace(score, n1 == 0 & n2 == 1, 2),
                  definition = replace(definition, n1 == 0 & n2 == 1, "gain of edge")) %>%
    dplyr::mutate(., score = replace(score, n1 == 1 & n2 == 1 & cor1 > 0 & cor2 < 0, 3),
                  definition = replace(definition, n1 == 1 & n2 == 1 & cor1 > 0 & cor2 < 0, "change of sign (pos -> neg)")) %>%
    dplyr::mutate(., score = replace(score, n1 == 1 & n2 == 1 & cor1 < 0 & cor2 > 0, 3),
                  definition = replace(definition, n1 == 1 & n2 == 1 & cor1 < 0 & cor2 > 0, "change of sign (neg -> pos)"))

  return(score_df)
}
