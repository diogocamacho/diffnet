# DELTA NET
# Looking at network differences based on correlation changes between variables
diffnet <- function(data,
                 group1,
                 group2,
                 correlation_type=c("pearson","spearman"),
                 correlation_thr,
                 p_thr,
                 variable_names)
{

  # if(!require(Hmisc,quietly = TRUE)) install.packages("Hmisc")

  # function argument checks ----
  if(missing(correlation_type)) correlation_type <- "spearman"
  if(missing(p_thr)) p_thr <- 0.05
  if(missing(correlation_thr)) correlation_thr <- 0.8
  # if(missing(variable_names) & !is.null(rownames(data))) {
  #   variable_names <- rownames(data)
  # } else {
  #   variable_names <- seq(1,nrow(data))
  # }
  if(missing(variable_names)) stop("Need variable names.")
  if(missing(group1) | missing(group2)) stop("Need group identification.")


  # compute correlations ----
  x1 <- rcorr(t(data[,group1]),type = correlation_type)
  r1 <- x1[[1]]
  pr1 <- x1[[3]]

  x2 <- rcorr(t(data[,group2]),type = correlation_type)
  r2 <- x2[[1]]
  pr2 <- x2[[3]]

  # define interactions ----
  interaction_ids <- which(upper.tri(pr1))
  g1 <- ceiling(interaction_ids / nrow(data))
  g2 <- interaction_ids %% nrow(data)

  # because it's correlations, upper triangular is sufficient
  r1 <- r1[interaction_ids]
  pr1 <- pr1[interaction_ids]
  r2 <- r2[interaction_ids]
  pr2 <- pr2[interaction_ids]

  # fix nas ----
  r1[which(is.na(r1))] <- 0
  pr1[which(is.na(r1))] <- 1
  r2[which(is.na(r2))] <- 0
  pr2[which(is.na(r2))] <- 1

  # multiple correction of p values ----
  adj_pr1 <- p.adjust(pr1,"fdr")
  adj_pr2 <- p.adjust(pr2,"fdr")

  # interactions ----
  n1 <- matrix(0,nrow=length(r1),1)
  n2 <- matrix(0,nrow=length(r2),1)

  n1[which(adj_pr1 < p_thr & abs(r1) > correlation_thr)] <- 1
  n2[which(adj_pr2 < p_thr & abs(r2) > correlation_thr)] <- 1

  # scoring ----
  # to decode things:
  # S = 1: edge is present in both networks with the same sign
  # S = 2: edge is present in only 1 network
  # S = 3: edge is present in both networks but sign changed
  # D: difference in correlation
  # C = 1: same sign
  # C = 2: loss of edge
  # C = 3: gain of edge
  # C = 4: change of sign
  # N = n1 + n2
  D <- abs(r1 - r2)
  S <- matrix(0,nrow=nrow(n1),1)
  C <- matrix(0,nrow=nrow(n1),1)

  S[which(n1 == 1 & n2 == 1 & r1 > 0 & r2 > 0)] = 1
  C[which(n1 == 1 & n2 == 1 & r1 > 0 & r2 > 0)] = 1 # edge present, same sign
  S[which(n1 == 1 & n2 == 1 & r1 < 0 & r2 < 0)] = 1
  C[which(n1 == 1 & n2 == 1 & r1 < 0 & r2 < 0)] = 1 # edge present, same sign

  S[which(n1 == 1 & n2 == 1 & r1 > 0 & r2 < 0)] = 3
  C[which(n1 == 1 & n2 == 1 & r1 > 0 & r2 < 0)] = 4 # edge present, change of sign
  S[which(n1 == 1 & n2 == 1 & r1 < 0 & r2 > 0)] = 3
  C[which(n1 == 1 & n2 == 1 & r1 < 0 & r2 > 0)] = 4 # edge present, change of sign

  S[which(n1 == 1 & n2 == 0)] = 2
  C[which(n1 == 1 & n2 == 0)] = 2 # loss of edge
  S[which(n1 == 0 & n2 == 1)] = 2
  C[which(n1 == 0 & n2 == 1)] = 3 # gain of edge


  # pvalue for differences ----
  xx1 <- 0.5 * log((1 + r1) / (1 - r1))
  xx2 <- 0.5 * log((1 + r2) / (1 - r2))

  df1 <- 1 / (length(group1) - 3)
  df2 <- 1 / (length(group2) - 3)

  diff <- xx1 - xx2

  z <- diff / sqrt(df1 + df2)

  p_val <- 2 * pnorm(-abs(z))
  logPval <- -log10(p_val)
  logPval[logPval == Inf] <- ceiling(max(logPval[logPval != Inf]))

  # clean interactions ----
  # ignore interactions with score 0
  nix <- which(S == 0)
  nix <- union(nix,which(D == 0))
  # N <- N[-nix]
  S <- S[-nix]
  C <- C[-nix]
  D <- D[-nix]
  p_val <- p_val[-nix]
  logPval <- logPval[-nix]
  r1 <- r1[-nix]
  pr1 <- pr1[-nix]
  r2 <- r2[-nix]
  pr2 <- pr2[-nix]
  interaction_ids <- interaction_ids[-nix]
  g1 <- g1[-nix]
  g2 <- g2[-nix]

  # var.ints <- paste(varNames[g1]," :: ",varNames[g2],sep="")
  total_score <- abs(r1) + abs(r2) + S + C + logPval

  X <- data_frame(node1 = variable_names[g1],
                  node2 = variable_names[g2],
                  correlation_group1 = r1,
                  correlation_group2 = r2,
                  correlation_difference = D,
                  difference_pval = p_val,
                  edge_score = S,
                  edge_class = C,
                  logPval = logPval,
                  total_score = total_score)

  return(X)
}
