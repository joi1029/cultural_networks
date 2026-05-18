rm(list = ls())

# Change working directory as needed
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")
library(dplyr)
library(pbapply)
library(parallel)


# Step 1. Calculate 2SD thresholds for each j in each year, over all bootstrap distributions ================
load("yearlist.saved")
years <- yearlist

# Process each year's bootstrap file
all_thresholds_list <- list()

for(year in years) {
  filename <- paste("bootstrapped_corrs_500_0312", year, ".saved", sep = "")
  if(file.exists(filename)) {
    load(filename)  # loads b_corrs - list of dataframes (one per bootstrap rep)
    
    # combine all bootstrap for a year year
    year_boot_data <- bind_rows(b_corrs_filtered, .id = "boot_rep")
    year_boot_data$year <- year
    
    # calculate s.d. of abs_c for each j in the year
    year_thresholds <- year_boot_data %>%
      group_by(j, year) %>%
      summarise(threshold_2sd = 2 * sd(abs_c, na.rm = TRUE), .groups = "drop")
    
    all_thresholds_list[[as.character(year)]] <- year_thresholds
    cat("Year", year, ": calculated thresholds for", nrow(year_thresholds), "pairs\n")
  }
}

# combine all years
all_thresholds <- bind_rows(all_thresholds_list)
cat("Total thresholds calculated for", nrow(all_thresholds), "unique (j, year) pairs\n")
head(all_thresholds)

save(all_thresholds, file = "all_thresholds_0312_500.saved")


# Step 2. Load Predictions and apply filtering ==============================================================
load("all_thresholds_0312_500.saved")  # loads 'all_thresholds' dataframe
load("bootstrapped_pred_corrs_1-500_0312.saved")  # loads 'filtered_results' - list of dataframes
length(results)


n_cores <- 5
cl <- makeCluster(n_cores)
clusterExport(cl, c("all_thresholds", "results"))

# Function to process a single bootstrap replicate
process_bootstrap <- function(i) {
  df <- results[[i]]

  df <- merge(df, all_thresholds, by = c("j", "year"), all.x = TRUE)
  has_threshold <- !is.na(df$threshold_2sd)
  
  # Process c_obs observed values
  c_obs_mask <- has_threshold & !is.na(df$c_obs) & abs(df$c_obs) < df$threshold_2sd
  filtered_c_obs_count <- sum(c_obs_mask, na.rm = TRUE)
  df$c_obs[c_obs_mask] <- 0
  
  # Process c_est predicted values (where c_obs is NA)
  c_est_mask <- has_threshold & is.na(df$c_obs) & abs(df$c_est) < df$threshold_2sd
  filtered_c_est_count <- sum(c_est_mask, na.rm = TRUE)
  df$c_est[c_est_mask] <- 0
  
  # Get pairs that were filtered
  pairs_filtered <- unique(c(df$j[c_obs_mask], df$j[c_est_mask]))
  
  return(list(
    data = df,
    filtered_c_obs = filtered_c_obs_count,
    filtered_c_est = filtered_c_est_count,
    filtered_pairs = pairs_filtered
  ))
}

# Parallel Process
cat("Starting parallel processing with", n_cores, "cores...\n")
start_time <- Sys.time()

parallel_results <- pblapply(seq_along(results), process_bootstrap, cl = cl)

stopCluster(cl)

# Extract results
filtered_results <- lapply(parallel_results, `[[`, "data")
total_filtered_c_obs <- sum(sapply(parallel_results, `[[`, "filtered_c_obs"))
total_filtered_c_est <- sum(sapply(parallel_results, `[[`, "filtered_c_est"))
filtered_pairs <- lapply(parallel_results, `[[`, "filtered_pairs")
all_filtered_pairs <- unique(unlist(filtered_pairs))

end_time <- Sys.time()


cat("Parallel processing completed in:", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")
cat("Total c_obs filtered:", total_filtered_c_obs, "\n")
cat("Total c_est filtered:", total_filtered_c_est, "\n")
cat("Total unique pairs filtered:", length(all_filtered_pairs), "\n")



# Save filtered results for plotting
save(filtered_results, file = "bootstrapped_pred_corrs_500_0312_filtered_by_se.saved")