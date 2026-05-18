## AIC Test for Non-linearity Over Time (Linear vs. Quadratic Polynomial Regression)
## Network-level and Node-level Metrics

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

getwd()
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")

# =====================================================================
# Part 1: Network Level AIC Test
load("Dens_full.saved")                                    # Density matrix
load("max_kcore_summary_bootstrapped_garas.saved")        # Max k-core summary
load("yearlist.saved")

# Prepare Density data
dens_df <- data.frame(
  year = Dens[, 1],
  mean_density = rowMeans(Dens[, -1], na.rm = TRUE),
  se_density = apply(Dens[, -1], 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
)

# Prepare Max K-core data
kcore_df <- max_kcore_summary_bootstrapped %>%
  dplyr::select(year, mean_max_k, se_max_k)

cat("Density data summary:\n")
print(head(dens_df))
cat("\nMax K-core data summary:\n")
print(head(kcore_df))

# Filter for post 1990 data:
dens_df <- dens_df %>% filter(year >= 1990)
kcore_df <- kcore_df %>% filter(year >= 1990)

# Function to fit linear and quadratic models and compare with AIC
fit_aic_comparison <- function(data, metric_name, year_col = "year", value_col = "mean_value") {
  
  # Ensure data is sorted by year
  data <- data[order(data[[year_col]]), ]
  
  # Fit linear model: y ~ year
  model_linear <- lm(as.formula(paste(value_col, "~ ", year_col)), data = data)
  aic_linear <- AIC(model_linear)
  bic_linear <- BIC(model_linear)
  
  # Fit quadratic model: y ~ year + year^2
  model_quad <- lm(as.formula(paste(value_col, "~ poly(", year_col, ", 2, raw = TRUE)")), data = data)
  aic_quad <- AIC(model_quad)
  bic_quad <- BIC(model_quad)
  
  aic_diff <- aic_linear - aic_quad  # positive means quadratic is better
  bic_diff <- bic_linear - bic_quad
  
  lr_test <- anova(model_linear, model_quad)
  
  # Extract R-squared values
  r2_linear <- summary(model_linear)$r.squared
  r2_quad <- summary(model_quad)$r.squared
  adj_r2_linear <- summary(model_linear)$adj.r.squared
  adj_r2_quad <- summary(model_quad)$adj.r.squared
  
  # Label the beter model
  better_model_aic <- ifelse(aic_quad < aic_linear, "Quadratic", "Linear")
  better_model_bic <- ifelse(bic_quad < bic_linear, "Quadratic", "Linear")
  
  summary_list <- list(
    metric = metric_name,
    n_years = nrow(data),
  
    aic_linear = aic_linear,
    bic_linear = bic_linear,
    r2_linear = r2_linear,
    adj_r2_linear = adj_r2_linear,
    model_linear = model_linear,
    
    aic_quad = aic_quad,
    bic_quad = bic_quad,
    r2_quad = r2_quad,
    adj_r2_quad = adj_r2_quad,
    model_quad = model_quad,
    
    aic_diff = aic_diff,
    bic_diff = bic_diff,
    delta_r2 = r2_quad - r2_linear,
    lr_statistic = lr_test$`F`[2],
    lr_pvalue = lr_test$`Pr(>F)`[2],
    
    better_model_aic = better_model_aic,
    better_model_bic = better_model_bic
  )
  
  return(summary_list)
}

# Run AIC comparison for Density
dens_df_clean <- dens_df %>%
  rename(mean_value = mean_density) %>%
  dplyr::select(year, mean_value)

density_aic <- fit_aic_comparison(dens_df_clean, "Density", year_col = "year", value_col = "mean_value")

# Run AIC comparison for Max K-core
kcore_df_clean <- kcore_df %>%
  rename(mean_value = mean_max_k) %>%
  dplyr::select(year, mean_value)

kcore_aic <- fit_aic_comparison(kcore_df_clean, "Max_K-core", year_col = "year", value_col = "mean_value")


# Create summary table
network_aic_summary <- data.frame(
  Metric = c("Density", "Max K-core"),
  Linear_AIC = c(round(density_aic$aic_linear, 3), round(kcore_aic$aic_linear, 3)),
  Quadratic_AIC = c(round(density_aic$aic_quad, 3), round(kcore_aic$aic_quad, 3)),
  AIC_Diff = c(round(density_aic$aic_diff, 3), round(kcore_aic$aic_diff, 3)),
  Linear_R2 = c(round(density_aic$r2_linear, 4), round(kcore_aic$r2_linear, 4)),
  Quadratic_R2 = c(round(density_aic$r2_quad, 4), round(kcore_aic$r2_quad, 4)),
  Delta_R2 = c(round(density_aic$delta_r2, 4), round(kcore_aic$delta_r2, 4)),
  LR_Pvalue = c(round(density_aic$lr_pvalue, 4), round(kcore_aic$lr_pvalue, 4)),
  Better_Model = c(density_aic$better_model_aic, kcore_aic$better_model_aic)
)

print(network_aic_summary)
save(network_aic_summary, file = "network_aic_summary.RData")
save(density_aic, file = "density_aic_results.RData")
save(kcore_aic, file = "kcore_aic_results.RData")


# =====================================================================
# Part 2: Node-level AIC Test
# Load node-level metrics
load("bootstrap_summary_stats_500_0312.RData")  # node_summary data.frame

cat("Node summary data structure:\n")
print(str(node_summary))
cat("\nSample node summary:\n")
print(head(node_summary))

# Prepare node-level data by reshaping to long format
node_long <- node_summary %>%
  pivot_longer(
    cols = starts_with(c("betweenness_", "degree_", "closeness_")),
    names_to = "metric_type",
    values_to = "value"
  ) %>%
  separate(metric_type, into = c("metric_base", "stat_type"), sep = "_(?=mean|sd|q)")

# Extract just the mean values for AIC testing
node_means <- node_summary %>%
  dplyr::select(node, year, betweenness_mean, degree_mean, closeness_mean) %>%
  pivot_longer(
    cols = c(betweenness_mean, degree_mean, closeness_mean),
    names_to = "metric",
    values_to = "mean_value"
  ) %>%
  mutate(metric = sub("_mean", "", metric))

unique_nodes <- unique(node_means$node)
unique_metrics <- unique(node_means$metric)

cat("Number of nodes:", length(unique_nodes), "\n")
cat("Metrics tested:", paste(unique_metrics, collapse = ", "), "\n")

# Initialize list to store results
node_aic_results <- list()

# Loop through each node and metric
for (metric in unique_metrics) {
  
  # Filter data for this metric
  metric_data <- node_means %>%
    filter(metric == !!metric)
  
  # Initialize vectors for this metric
  nodes_tested <- vector("character")
  linear_aic_vals <- vector("numeric")
  quad_aic_vals <- vector("numeric")
  aic_diffs <- vector("numeric")
  linear_r2_vals <- vector("numeric")
  quad_r2_vals <- vector("numeric")
  delta_r2_vals <- vector("numeric")
  lr_pvals <- vector("numeric")
  better_models <- vector("character")
  
  # Loop through each node
  for (node in unique_nodes) {
    
    node_data <- metric_data %>%
      filter(node == !!node) %>%
      dplyr::select(year, mean_value) %>%
      arrange(year)
    
    # Skip if insufficient data (need at least 3 points)
    if (nrow(node_data) < 3) {
      next
    }
    
    tryCatch({
      # Fit models
      model_lin <- lm(mean_value ~ year, data = node_data)
      model_qd <- lm(mean_value ~ poly(year, 2, raw = TRUE), data = node_data)
      
      # Get AICs
      aic_lin <- AIC(model_lin)
      aic_qd <- AIC(model_qd)
      aic_diff <- aic_lin - aic_qd
      
      # Get R^2 values
      r2_lin <- summary(model_lin)$r.squared
      r2_qd <- summary(model_qd)$r.squared
      delta_r2 <- r2_qd - r2_lin
      
      # Likelihood ratio test
      lr_test <- anova(model_lin, model_qd)
      lr_pval <- lr_test$`Pr(>F)`[2]
      
      # Determine better model
      better <- ifelse(aic_qd < aic_lin, "Quadratic", "Linear")
      
      nodes_tested <- c(nodes_tested, node)
      linear_aic_vals <- c(linear_aic_vals, aic_lin)
      quad_aic_vals <- c(quad_aic_vals, aic_qd)
      aic_diffs <- c(aic_diffs, aic_diff)
      linear_r2_vals <- c(linear_r2_vals, r2_lin)
      quad_r2_vals <- c(quad_r2_vals, r2_qd)
      delta_r2_vals <- c(delta_r2_vals, delta_r2)
      lr_pvals <- c(lr_pvals, lr_pval)
      better_models <- c(better_models, better)
      
    }, error = function(e) {
      cat("  Error for node ", node, ": ", e$message, "\n", sep = "")
    })
  }
  
  # Create summary dataframe for this metric
  node_metric_summary <- data.frame(
    node = nodes_tested,
    metric = metric,
    Linear_AIC = round(linear_aic_vals, 3),
    Quadratic_AIC = round(quad_aic_vals, 3),
    AIC_Diff = round(aic_diffs, 3),
    Linear_R2 = round(linear_r2_vals, 4),
    Quadratic_R2 = round(quad_r2_vals, 4),
    Delta_R2 = round(delta_r2_vals, 4),
    LR_Pvalue = round(lr_pvals, 4),
    Better_Model = better_models,
    stringsAsFactors = FALSE
  )
  
  node_aic_results[[metric]] <- node_metric_summary
  
  # Print summary stats for this metric
  cat("Nodes tested:", length(nodes_tested), "\n")
  cat("Quadratic better (AIC):", sum(better_models == "Quadratic"), "\n")
  cat("Linear better (AIC):", sum(better_models == "Linear"), "\n")
  cat("Avg AIC Difference:", round(mean(aic_diffs), 3), "\n")
  cat("Avg ΔR²:", round(mean(delta_r2_vals), 4), "\n")
  cat("% nodes with p < 0.05:", round(sum(lr_pvals < 0.05) / length(lr_pvals) * 100, 1), "%\n")
  
  # Print top nodes where quadratic fits significantly better
  top_quad <- node_metric_summary %>%
    filter(Better_Model == "Quadratic") %>%
    arrange(desc(AIC_Diff)) %>%
    slice_head(n = 10)
  
  if (nrow(top_quad) > 0) {
    cat("\nTop nodes where Quadratic fits better:\n")
    print(top_quad %>% dplyr::select(node, AIC_Diff, LR_Pvalue))
  }
}

# Combine all node results
all_node_aic_summary <- bind_rows(node_aic_results)

# Node-level AIC summary
cat("Total node-metric combinations tested:", nrow(all_node_aic_summary), "\n")
cat("Quadratic better (AIC):", sum(all_node_aic_summary$Better_Model == "Quadratic"), "\n")
cat("Linear better (AIC):", sum(all_node_aic_summary$Better_Model == "Linear"), "\n")

# Summary by metric
node_summary_by_metric <- all_node_aic_summary %>%
  group_by(metric) %>%
  summarise(
    n_nodes = n(),
    quadratic_better_count = sum(Better_Model == "Quadratic"),
    quadratic_better_pct = round(sum(Better_Model == "Quadratic") / n() * 100, 1),
    mean_aic_diff = round(mean(AIC_Diff), 3),
    mean_delta_r2 = round(mean(Delta_R2), 4),
    sig_lr_test_pct = round(sum(LR_Pvalue < 0.05) / n() * 100, 1),
    .groups = "drop"
  )

cat("\n===== NODE-LEVEL SUMMARY BY METRIC =====\n")
print(node_summary_by_metric)

# Save results
save(all_node_aic_summary, file = "node_aic_summary_all.RData")
save(node_summary_by_metric, file = "node_aic_summary_by_metric.RData")
save(node_aic_results, file = "node_aic_results_detailed.RData")



# ======================================================================
# Table S2: Extract results for polviews and partyid nodes particularly
target_nodes <- c("polviews", "partyid")
target_metrics <- c("betweenness", "degree")

# Create summary table for selected specifici nodes (i.e. party, ideology)
specific_nodes_summary <- all_node_aic_summary %>%
  filter(node %in% target_nodes & metric %in% target_metrics) %>%
  dplyr::select(node, metric, Linear_AIC, Quadratic_AIC, AIC_Diff, 
                Linear_R2, Quadratic_R2, Delta_R2, LR_Pvalue, Better_Model)

print(specific_nodes_summary)
save(specific_nodes_summary, file = "polviews_partyid_aic_results.RData")