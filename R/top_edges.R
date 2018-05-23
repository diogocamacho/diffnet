# EXTRACT TOP EDGES
# From a DNet result, get the top edges that highlight difference between network topologies
top_edges <- function(diffnet_results,ulim,blim,topN)
{

  if(missing(ulim)) ulim <- 0.8
  if(missing(blim)) blim <- 0.25
  if(missing(topN)) topN <- 500

  gl_edges <- diffnet_results %>%
    dplyr::filter(.,abs(correlation_group1) > ulim | abs(correlation_group2) > ulim) %>%
    dplyr::filter(.,abs(correlation_group1) < blim | abs(correlation_group2) < blim) %>%
    dplyr::arrange(.,desc(total_score)) %>%
    dplyr::select(.,node1,node2,edge_score,edge_class,total_score)

  # change of direction
  # dd_edges <- diffnet_results %>%
  #   dplyr::filter(score_class == 4)

  # put data frame back together
  # redDN <- rbind(gl_edges,dd_edges)
  # redDN <- arrange(redDN,desc(total.score))
  # redDN <- slice(redDN,row_number(1:topN))

  return(gl_edges)
}
