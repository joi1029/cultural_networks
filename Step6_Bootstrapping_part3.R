#Generating measures of network time trends using bootstrapped data 
# Density, Kcore
rm(list=ls())

getwd()
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")

library(igraph)
library(ineq)
library(parallel)
library(pbapply)
library(dplyr)
library(purrr)
library(ggplot2)

load("Z:/jc3528/OilSpill/CultureNetwork_0312/bootstrapped_pred_corrs_500_0312_filtered_by_se.saved")
load(file="modelinput_0312.saved")
load(file="yearlist.saved")

#####################################################
# Parallel processing step

# Set up parallel cluster
n_cores <- 4 #do not load too many, code below loads large object into each core
cat("Using", n_cores, "cores for parallel processing\n")
cl <- makeCluster(n_cores)

# Load libraries on each worker
clusterEvalQ(cl, {
  library(igraph)
  library(ineq)
})

# Export large objects ONCE to all workers (not inside function calls)
clusterExport(cl, varlist = c("filtered_results", "r", "yearlist"), envir = environment())

# process a single bootstrap replication to get network level metrics for each year
process_bootstrap_rep <- function(i) {
  
  # Set up master edgelist across all years 
  e = filtered_results[[i]]
  e$x = r$x[match(e$j, r$j)]
  e$y = r$y[match(e$j, r$j)]
  
  e$weight = ifelse(is.na(e$c_obs), e$c_est, e$c_obs)
  
  e$weight[e$weight<0]=0 #bound weights between 0 and 1
  e$weight[e$weight>1]=1
  
  # Initialize result vectors for this replication
  n_years <- length(yearlist)
  Dens_i <- numeric(n_years)
  
  # Record network properties for each year
  for(j in 1:n_years) {
    y <- yearlist[j]
    sub = e[e$year==y,]
    g = graph_from_data_frame(sub[, c("x", "y", "weight")], directed=FALSE)
    g = delete_edges(g, E(g)[weight <= 0])
    
    c = cluster_walktrap(g, weights = E(g)$weight)
    V(g)$com = c$membership
    
    com_table <- table(V(g)$com)
    sorted_com <- sort(com_table, decreasing=TRUE)

    Dens_i[j] = sum(E(g)$weight) / ((vcount(g)*(vcount(g)-1)))
  }
  
  return(list(Dens=Dens_i))
}

# Export the function for parallel processing
clusterExport(cl, varlist = c("process_bootstrap_rep"), envir = environment())

# Run parallel processing with progress bar
results_list <- pblapply(1:reps, function(i) {
  process_bootstrap_rep(i)
}, cl = cl)

# Initialize matrices
Dens = matrix(NA, nrow=length(yearlist), ncol = reps + 1)

# First column is yearlist
Dens[,1] = yearlist

# Fill in results from parallel computation
for(i in 1:reps) {
  Dens[,i+1] = results_list[[i]]$Dens
}

# Save results
save(Dens, file="Dens_full.saved")

# Stop cluster when done
stopCluster(cl)



# ========================================================================================================
# K-core Calculation
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")
load("Z:/jc3528/OilSpill/CultureNetwork_0312/bootstrapped_pred_corrs_500_0312_filtered_by_se.saved") #called filtered_results


reps <- length(filtered_results)

# Function to calculate weighted kcore
get_weighted_kcores <- function(
  g,
  max_k = NULL,
  alpha = 1,
  beta  = 1,
  mode = c("all", "in", "out"),
  weight_attr = "weight"
) {
  mode <- match.arg(mode)
  if (!igraph::is_igraph(g)) {
    stop("g must be an igraph object.")
  }
  if (!weight_attr %in% igraph::edge_attr_names(g)) {
    stop("Weight attribute not found.")
  }
  empty_result <- tibble::tibble(
    k = numeric(0),
    survivors = list(),
    core_graph = list()
  )
  # Use raw correlation weights directly
  w <- igraph::edge_attr(g, weight_attr)
  if (any(is.na(w) | is.nan(w))) {
    stop("Edge weights contain NA/NaN.")
  }
  if (any(w < 0)) {
    stop("Edge weights must be nonnegative.")
  }
  inv_ab <- 1 / (alpha + beta)

  # Garas-style weighted degree:
  # k_i^(w) = (deg_i^alpha * str_i^beta)^(1/(alpha + beta))
  weighted_degree <- function(h) {
    deg <- igraph::degree(h, mode = mode)
    s   <- igraph::strength(h, mode = mode, weights = igraph::edge_attr(h, weight_attr))
    (deg^alpha * s^beta)^inv_ab
  }

  # Determine the maximum k threshold from the full graph
  if (is.null(max_k)) {
    kp0 <- weighted_degree(g)
    max_k <- floor(max(kp0, na.rm = TRUE))
  }
  if (max_k < 1) {
    return(empty_result)
  }
  results <- vector("list", max_k)
  current_g <- g

  for (k in seq_len(max_k)) {
    repeat {
      if (igraph::vcount(current_g) == 0) break
      kprime <- weighted_degree(current_g)
      if (any(is.na(kprime) | is.nan(kprime))) {
        warning("NA/NaN in weighted degree at k = ", k, ". Keeping current survivors.")
        break
      }
      remove_idx <- which(kprime < k)
      if (length(remove_idx) == 0) break
      current_g <- igraph::delete_vertices(current_g, remove_idx)
    }

    if (igraph::vcount(current_g) == 0) break

    results[[k]] <- tibble::tibble(
      k = k,
      survivors = list(igraph::V(current_g)$name),
      core_graph = list(current_g)
    )
  }
  results <- results[!vapply(results, is.null, logical(1))]
  if (length(results) == 0) {
    return(empty_result)
  }
  dplyr::bind_rows(results)
}



# Parallel K-core
n_cores <- 4
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(igraph)
  library(dplyr)
  library(tidyr)
})
clusterExport(cl, c("filtered_results", "yearlist", "get_weighted_kcores"), envir = environment())

# Function to process one bootstrap sample
process_one_bootstrap <- function(i) {
  e <- filtered_results[[i]]
  e$weight <- ifelse(is.na(e$c_obs), e$c_est, e$c_obs)
  e$weight[e$weight < 0] <- 0
  e$weight[e$weight > 1] <- 1

  e$x <- sub("_.*", "", e$j)
  e$y <- sub(".*_", "", e$j)
  
  max_k_vec <- numeric(length(yearlist))
  nodes_vec <- numeric(length(yearlist))
  density_vec <- numeric(length(yearlist))
  node_data_list <- vector("list", length(yearlist))
  
  for (j in seq_along(yearlist)) {
    y <- yearlist[j]
    sub <- e[e$year == y & e$weight > 1e-6, ]
   
    if (nrow(sub) > 0) {
      g <- graph_from_data_frame(sub[, c("x", "y", "weight")], directed = FALSE)
      cores_df <- get_weighted_kcores(g)
    
      if (nrow(cores_df) > 0) {
        max_k_value <- max(cores_df$k)
        max_k_row <- cores_df[cores_df$k == max_k_value, ]

        max_k_vec[j] <- max_k_value
        nodes_vec[j] <- length(max_k_row$survivors[[1]])
        density_vec[j] <- edge_density(max_k_row$core_graph[[1]])
        
        # Efficient node-level max k-core: iterate from highest k down,
        # assign nodes their highest k (since k-cores are nested)
        assigned <- character(0)
        node_names <- character(0)
        node_kcores <- integer(0)
        for (r in rev(seq_len(nrow(cores_df)))) {
          s <- cores_df$survivors[[r]]
          new_nodes <- setdiff(s, assigned)
          if (length(new_nodes) > 0) {
            node_names <- c(node_names, new_nodes)
            node_kcores <- c(node_kcores, rep(cores_df$k[r], length(new_nodes)))
            assigned <- c(assigned, new_nodes)
          }
        }
        node_data_list[[j]] <- data.frame(
          node = node_names, max_kcore = node_kcores,
          year = as.integer(y), stringsAsFactors = FALSE
        )
      } else {
        max_k_vec[j] <- 0; nodes_vec[j] <- 0; density_vec[j] <- 0
      }
    } else {
      max_k_vec[j] <- 0; nodes_vec[j] <- 0; density_vec[j] <- 0
    }
  }
  node_data <- bind_rows(node_data_list)
  if (nrow(node_data) > 0) node_data$bootstrap_id <- i
  
  list(max_k = max_k_vec, nodes = nodes_vec, density = density_vec, node_data = node_data)
}

# Quick test on ten bootstraps
#results <- pblapply(1:10, process_one_bootstrap)
#str(results[[1]])
#summary(results[[1]]$max_k)


# Run parallel processing
cat("Running parallel k-core analysis on", n_cores, "cores...\n")
results <- pblapply(1:500, process_one_bootstrap, cl = cl)
stopCluster(cl)

reps <- 500
# Combine results into matrices
Max_k <- matrix(NA, nrow = length(yearlist), ncol = reps + 1)
Nodes_in_maxk <- matrix(NA, nrow = length(yearlist), ncol = reps + 1)
Max_kcore_density <- matrix(NA, nrow = length(yearlist), ncol = reps + 1)
Max_k[,1] <- yearlist; Nodes_in_maxk[,1] <- yearlist; Max_kcore_density[,1] <- yearlist

length(results)
length(Max_k)


colnames(results[[1]])
reps <- 500
all_node_kcores <- list()
for (i in 1:reps) {
  Max_k[, i+1] <- results[[i]]$max_k
  Nodes_in_maxk[, i+1] <- results[[i]]$nodes
  Max_kcore_density[, i+1] <- results[[i]]$density
  if (nrow(results[[i]]$node_data) > 0) all_node_kcores[[i]] <- results[[i]]$node_data
}

head(results[[1]])

# Save intermediate matrices
save(Max_k, file = "Max_k_full.saved")
save(Nodes_in_maxk, file = "Nodes_in_maxk_full.saved")
save(Max_kcore_density, file = "Max_kcore_density_full.saved")


# Convert matrices to dataframes
bootstrap_max_kcore_summaries <- lapply(1:reps, function(i) {
  data.frame(
    year = yearlist,
    max_k = Max_k[, i+1],
    nodes_in_maxk = Nodes_in_maxk[, i+1],
    max_kcore_density = Max_kcore_density[, i+1],
    bootstrap_id = i
  )
})

# Combine all bootstrap summaries
all_bootstrap_summaries <- bind_rows(bootstrap_max_kcore_summaries)
cat("Total rows in max k-core summary:", nrow(all_bootstrap_summaries), "\n")
head(all_bootstrap_summaries)

# Combine all node-level k-core data
all_bootstrap_node_kcores <- bind_rows(all_node_kcores)
cat("Total rows in node k-core data:", nrow(all_bootstrap_node_kcores), "\n")
getwd()

# Save the summary data
save(bootstrap_max_kcore_summaries, file = "bootstrap_max_kcore_summaries_500.saved") #all 500 bootstraps as list
save(all_bootstrap_summaries, file = "all_bootstrap_summaries_combined.saved") #all 500 bootstraps combined as simgle list
save(all_bootstrap_node_kcores, file = "all_bootstrap_node_kcores_combined.saved")  # node-level information and kcore


# Summary mean and s.e. of kcore acros bootstraps by year
max_kcore_summary_bootstrapped <- all_bootstrap_summaries %>%
  group_by(year) %>%
  dplyr::summarise(
    max_k_mean = mean(max_k, na.rm = TRUE),
    max_k_se = sd(max_k, na.rm = TRUE),
    n_bootstraps = n(),
    .groups = "drop"
  )

save(max_kcore_summary_bootstrapped, file = "max_kcore_summary_bootstrapped.saved")