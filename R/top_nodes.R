# TOP CONNECTED NODES
# get the nodes that have highest number of edges
top_nodes <- function(diffnet_results,minimum_interactions)
{
  if(missing(minimum_interactions)) minimum_interactions <- 5

  tot_genes <- union(diffnet_results$node1,diffnet_results$node2)
  gene_count <- tot_genes %>%
    as.matrix %>%
    apply(1,function(x) length(which(diffnet_results$node1 == x)) + length(which(diffnet_results$node2 == x)))

  # most connected genes
  tgs <- tot_genes[gene_count >= minimum_interactions]
  tgs_num <- gene_count[gene_count >= minimum_interactions]
  conn_genes <- data.frame(gene_name=tgs,
                           number_edges=tgs_num)

  return(conn_genes)
}
