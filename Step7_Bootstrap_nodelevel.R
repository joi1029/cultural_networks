## Compute node-level metrics for every node, each year, each bootstrap
rm(list = ls())
ls()

# Load libraries
library(igraph)
library(MASS)
library(dplyr)
library(tidyr)
library(pbapply)
library(parallel)

# set work directory
getwd()
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")

# Load data
load("Z:/jc3528/OilSpill/CultureNetwork_0312/modelinput_0312.saved")              # r: data.frame with j, x, y, year, c_obs, c_est, etc.
load("Z:/jc3528/OilSpill/CultureNetwork_0312/bootstrapped_pred_corrs_500_0312_filtered_by_se.saved")
load("yearlist.saved")

boot_preds <- filtered_results
length(boot_preds)
tail(boot_preds[[1]])

rm(filtered_results)


# Ensure identifier columns are character for matching
r$j <- as.character(r$j)
for(i in seq_along(boot_preds)) {
  boot_preds[[i]]$j <- as.character(boot_preds[[i]]$j)
}

# Master node list
nodes <- unique(c(r$x, r$y))


# Initialize list to hold results per bootstrap
node_metrics_list <- vector("list", length(boot_preds))
names(node_metrics_list) <- paste0("boot", seq_along(boot_preds))


# Function to process a single bootstrap
process_bootstrap <- function(i, boot_preds, r, nodes, yearlist) {
  e0 = boot_preds[[i]]
  e0$x = r$x[match(e0$j, r$j)]
  e0$y = r$y[match(e0$j, r$j)]
 
  # Choose full condition weights
  e0$weight <- ifelse(is.na(e0$c_obs), e0$c_est, e0$c_obs)
  e0$weight[e0$weight<0]=0 
  e0$weight[e0$weight>1]=1
  
  this_df <- expand.grid(
    node = nodes,
    year = yearlist,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(
      betweenness = NA_real_,
      degree      = NA_real_,
      closeness  = NA_real_ 
    )

  #record network properties for each year
  for(y in yearlist) {
    sub = e0[e0$year==y,] 
    # Remove edges where both x and y are demographic nodes
    g = graph_from_data_frame(sub[, c("x", "y", "weight")], directed=FALSE)
    g <- delete_edges(g, E(g)[E(g)$weight <= 0])

    c = cluster_walktrap(g, weights = E(g)$weight)
    V(g)$com = c$membership

    #Compute metrics
    strg  <- igraph::strength(g, weights = E(g)$weight)
    betw  <- igraph::betweenness(
      g,
      directed = FALSE,
      weights = 1 / E(g)$weight
    )

    names(betw) <- V(g)$name
    names(strg) <- V(g)$name

    
    # assign metrics to this year's rows in the df
    rows_y <- which(this_df$year == y)
    #For nodes not in the graph, assign 0 (they don't exist in this bootstrap)
    this_df$betweenness[rows_y] <- ifelse(
      this_df$node[rows_y] %in% names(betw),
      betw[this_df$node[rows_y]],
      0
    )
    this_df$degree[rows_y] <- ifelse(
      this_df$node[rows_y] %in% names(strg),
      strg[this_df$node[rows_y]],
      0
    )
  }
  return(this_df)
}

# Set up parallel cluster
n_cores <- 4
cl <- makeCluster(n_cores)

# Export necessary objects to cluster
clusterExport(cl, c("r", "nodes", "yearlist", "process_bootstrap", "boot_preds"), envir = environment())
clusterEvalQ(cl, {
  library(igraph)
  library(dplyr)
})

# Process all bootstraps in parallel with progress bar
node_metrics_list <- pblapply(
  seq_along(boot_preds),
  function(i) process_bootstrap(i, boot_preds, r, nodes, yearlist),
  cl = cl
)

# Stop cluster
stopCluster(cl)

names(node_metrics_list) <- paste0("boot", seq_along(boot_preds))

head(node_metrics_list[[1]])
length(node_metrics_list)
str(node_metrics_list)
save(node_metrics_list, file = "node_metrics_list_500_0312.RData")


load("node_metrics_list_500_0312.RData")

# Calculate summary statistics across all bootstraps
calculate_bootstrap_summary <- function(node_metrics_list) {
  all_data <- do.call(rbind, node_metrics_list)
  
  summary_stats <- all_data %>%
    dplyr::group_by(node, year) %>%
    dplyr::summarise(
      betweenness_mean = mean(betweenness, na.rm = TRUE),
      betweenness_sd = sd(betweenness, na.rm = TRUE),
      betweenness_q025 = quantile(betweenness, 0.025, na.rm = TRUE),
      betweenness_q975 = quantile(betweenness, 0.975, na.rm = TRUE),
      betweenness_upper2se = betweenness_mean + 2 * betweenness_sd,
      betweenness_lower2se = betweenness_mean - 2 * betweenness_sd,

      degree_mean = mean(degree, na.rm = TRUE),
      degree_sd = sd(degree, na.rm = TRUE),
      degree_q025 = quantile(degree, 0.025, na.rm = TRUE),
      degree_q975 = quantile(degree, 0.975, na.rm = TRUE),
      degree_upper2se = degree_mean + 2 * degree_sd,
      degree_lower2se = degree_mean - 2 * degree_sd
    )
  
  return(summary_stats)
}

node_summary <- calculate_bootstrap_summary(node_metrics_list)

save(node_summary, file = "bootstrap_summary_stats_500_0312.RData")