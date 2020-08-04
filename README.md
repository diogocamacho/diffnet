# DiffNet: Analyzing topological differences of networks

`DiffNet` is an R package that is aimed at identifying differences in network topology. The algorithm takes as input omics data corresponding to two different conditions, infers a network of interactions between all variables in the data set, and then computes the difference in (inferred) network topology between the two conditions. 

## Installation
The easiest way to install the `DiffNet` package is using the `devtools` package:

```
library(devtools)
devtools::install_github("diogocamacho/diffnet")
```

## Example
Using the example data set provided with the library, we will show how to run the algorithm. First, we'll start by loading the library:

```
library(diffnet)
```

Next, let's select a random set of samples to classify as `Group 1` and a second set as `Group 2`. We have 120 samples in the data set, so each group will have exactly 60 samples:

```
set.seed(1234)
g1 <- sample(x = seq_along(1:120), size = 60, replace = FALSE)
g2 <- setdiff(seq_along(1:120), g1)
```

With the groups defined, we can now run the algorithm. We will run it using the default parameter for correlation type (Spearman) and correlation threshold (0.75):

```
D <- diff_top(data = dataset$expression_data, group1 = g1, group2 = g2)
```

Running this will generate a tibble with 27,409 rows, where each row corresponds to an edge in the network and the corresponding difference score. Using the gene annotations provided with the example data:

```
D %>% 
  dplyr::mutate(x = dataset$gene_annotations$symbol[x]) %>% 
  dplyr::mutate(y = dataset$gene_annotations$symbol[y]) %>% 
  dplyr::select(., x, y, change_type, total_score)
 ```
 
 
