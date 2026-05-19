## AIC Test for Non-linearity Over Time (Linear vs. Quadratic Polynomial Regression)
## Network-level and Node-level Metrics

rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

getwd()
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")


# Node-level AIC Test
# Load node-level metrics
load("bootstrap_summary_stats_500_0312.RData")  # node_summary data.frame

cat("Node summary data structure:\n")
print(str(node_summary))
cat("\nSample node summary:\n")
print(head(node_summary))

# Prepare node-level data by reshaping to long format
node_long <- node_summary %>%
  pivot_longer(
    cols = starts_with(c("betweenness_", "degree_")),
    names_to = "metric_type",
    values_to = "value"
  ) %>%
  separate(metric_type, into = c("metric_base", "stat_type"), sep = "_(?=mean|sd|q)")

# Extract just the mean values for AIC testing
node_means <- node_summary %>%
  dplyr::select(node, year, betweenness_mean, degree_mean) %>%
  pivot_longer(
    cols = c(betweenness_mean, degree_mean),
    names_to = "metric",
    values_to = "mean_value"
  ) %>%
  mutate(metric = sub("_mean", "", metric))

unique_nodes <- unique(node_means$node)
unique_metrics <- unique(node_means$metric)


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