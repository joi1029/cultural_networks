#Creating plots and figures from bootstrap results
rm(list=ls())

setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")
setwd("Z:/jc3528/OilSpill/OilSpill-CultureNetwork-Publication")
library(ggplot2)
library(grid)
library(gmodels)
library(dplyr)
library(tidyr)
library(showtext)
library(sysfonts)

getwd()

#=============================================================================
# Figure 2: Network-Level Metrics Over Time (Density and K-core)
#=============================================================================
load(file="yearlist.saved")
load("max_kcore_summary_bootstrapped.saved")
max_kcore_summary <- max_kcore_summary_bootstrapped %>%
  dplyr::select(c(year, mean_max_k, se_max_k))

load("bootstrap_max_kcore_summaries_500.saved")

reps = 500 #number of bootstrap replications

# Create combined dataset for multiple measures
combined_data <- data.frame(yearlist = yearlist)

#=== DENSITY ===
measure = "Dens" 
load(file = paste(measure, "_full_nodemog.saved", sep=""))
measure_f = get(measure)

# Calculate yearly means and SE
combined_data$density_mean = NA
combined_data$density_se = NA
for(i in 1:length(yearlist)) {
  bootstrap_estimates = measure_f[i,2:(reps+1)]
  bootstrap_mean = mean(bootstrap_estimates)
  combined_data$density_mean[i] = bootstrap_mean
  deviations = bootstrap_estimates - bootstrap_mean
  sse = sum(deviations^2)
  variance = sse / (reps - 1)
  combined_data$density_se[i] = sqrt(variance)
}
combined_data$density_smooth = loess(combined_data$density_mean ~ combined_data$yearlist, span = 0.2)$fitted


# Single metrics: Density
single_metrics_raw <- combined_data %>%
  dplyr::select(yearlist, 
         Density_mean = density_mean, Density_smooth = density_smooth, Density_se = density_se,
        ) %>%
  tidyr::pivot_longer(cols = -yearlist,
               names_to = c("measure", "stat"),
               names_sep = "_(?=mean|smooth|se)",
               values_to = "value") %>%
  tidyr::pivot_wider(names_from = stat,
              values_from = value)

# Max K-core data
kcore_data_raw <- data.frame(
  yearlist = yearlist,
  max_kcore_mean = max_kcore_summary$mean_max_k,
  max_kcore_smooth = predict(loess(max_kcore_summary$mean_max_k ~ max_kcore_summary$year, span = 0.2)),
  max_kcore_se = max_kcore_summary$se_max_k
)

font_add("Helvetica", regular = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueMedium.otf", bold = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueBold.otf")
showtext_auto()


# Create the plots
# Plot 1: Density
p1_raw <- ggplot(single_metrics_raw[single_metrics_raw$measure == "Density", ], 
             aes(x = yearlist)) +
  geom_ribbon(aes(ymin = smooth - 2*se, ymax = smooth + 2*se),
              alpha = 0.3, fill = "grey50") +
  geom_point(aes(y = mean), size = 1, color = "black") +
  geom_line(aes(y = smooth), linewidth = 0.8, color = "black") +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  labs(title = "A", x = "", y = "Density") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0),        
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "black"),     
    axis.title.x = element_blank(),                   
    axis.title.y = element_text(size = 12, color = "black"),                   
    axis.text.x = element_text(size = 12, color = "black"),                    
    axis.text.y = element_text(size = 12, color = "black"),                    
    legend.title = element_text(size = 12, color = "black"),                   
    legend.text = element_text(size = 12, color = "black"),
    strip.text = element_text(size = 12, color = "black")
  )
windows()
p1_raw

head(kcore_data_raw)
unique(kcore_data_raw$yearlist)
# Plot 2: Max K-core (Raw Values)
p2_raw <- ggplot(kcore_data_raw, aes(x = yearlist)) +
  geom_ribbon(aes(ymin = max_kcore_smooth - 2*max_kcore_se, 
                  ymax = max_kcore_smooth + 2*max_kcore_se),
              alpha = 0.3, fill = "grey50") +
  geom_point(aes(y = max_kcore_mean), size = 1, color = "black") +
  geom_line(aes(y = max_kcore_smooth), linewidth = 0.8, color = "black") +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  labs(title = "B", x = "", y = expression("Max " * italic(k) * "-core")) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold", hjust = 0),        
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "black"),     
    axis.title.x = element_blank(),                   
    axis.title.y = element_text(size = 12, color = "black"),                   
    axis.text.x = element_text(size = 12, color = "black"),                    
    axis.text.y = element_text(size = 12, color = "black"),                    
    legend.title = element_text(size = 12, color = "black"),                   
    legend.text = element_text(size = 12, color = "black"),
    strip.text = element_text(size = 12, color = "black")
  )

library(patchwork)
# Combine plots (Raw Values)
combined_plot_raw <- (p1_raw | p2_raw) + 
  plot_annotation(
    theme = theme(
                  axis.text.x = element_blank())
  )

windows()
print(combined_plot_raw)

showtext_opts(dpi = 300)
mypath2 = "plots/fig2_points.tiff"
tiff(file = mypath2, width = 2800, height = 1000, res = 300, pointsize = 10, compression = "lzw") # 16:6 ratio
print(combined_plot_raw)
dev.off()




#=============================================================================
#Figure 3: Node Level Centrality (Betweenness and Degree)
#==============================================================================
library(patchwork)
library(ggplot2)
library(scales)
library(showtext)

load("bootstrap_summary_stats_500_0312.RData")
load("node_metrics_list_500_0312.RData")

summary(node_summary$betweenness_mean)
head(node_metrics_list[[1]], 2)
head(node_summary, 2)

font_add("Helvetica", regular = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueRoman.otf", bold = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueBold.otf")
showtext_auto()

# Define nodes to plot
nodes_to_plot <- c("educ", "age", "race", "polviews", "partyid")

node_summary <- node_summary %>%
  mutate(
    node_type = case_when(
      node == "partyid" ~ "Party",
      node == "polviews" ~ "Ideology", 
      node == "educ" ~ "Education",
      node == "age" ~ "Age",
      node == "race" ~ "Race",
      TRUE ~ "Other Issues"
    ),
    node_type = factor(node_type, levels = c(
      "Party", "Ideology", "Education", "Age", "Race"
    ))
  )

# Color palette
demo_colors <- c(
  "Party" = "#d83034",
  "Ideology" = "#000000",
  "Education" = "dark green",
  "Age" = "#ff7928",
  "Race" = "#008dff"
)

node_summary_filtered <- node_summary %>%
  filter(node %in% nodes_to_plot)

summary_long <- node_summary_filtered %>%
  dplyr::select(node, year, node_type,
                degree_mean, degree_upper2se, degree_lower2se,
                betweenness_mean, betweenness_upper2se, betweenness_lower2se) %>%
  pivot_longer(
    cols = c(degree_mean, degree_upper2se, degree_lower2se,
             betweenness_mean, betweenness_upper2se, betweenness_lower2se),
    names_to = c("metric", "stat"),
    names_pattern = "(.*)_(mean|upper2se|lower2se)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  dplyr::rename(lower = lower2se, upper = upper2se)

# Apply LOWESS smoothing to each node's centrality values
summary_long <- summary_long %>%
  group_by(node, metric) %>%
  mutate(
    smooth_mean = if(n() > 2) {
      loess_fit <- loess(mean ~ year, data = tibble(year = year, mean = mean), span = 0.2)
      predict(loess_fit, newdata = tibble(year = year))
    } else {
      mean
    },
    smooth_lower = if(n() > 2) {
      loess_fit <- loess(lower ~ year, data = tibble(year = year, lower = lower), span = 0.2)
      predict(loess_fit, newdata = tibble(year = year))
    } else {
      lower
    },
    smooth_upper = if(n() > 2) {
      loess_fit <- loess(upper ~ year, data = tibble(year = year, upper = upper), span = 0.2)
      predict(loess_fit, newdata = tibble(year = year))
    } else {
      upper
    }
  ) %>%
  ungroup()

# Betweenness plot
p_betweenness <- summary_long %>%
  filter(metric == "betweenness") %>%
  ggplot(aes(x = year, color = node_type, fill = node_type)) +
  geom_ribbon(aes(ymin = smooth_lower, ymax = smooth_upper), alpha = 0.2, color = NA) +
  geom_point(aes(y = mean, group = node), size = 0.6) +
  geom_line(aes(y = smooth_mean, group = node), size = 0.6) +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(
  breaks = seq(0, 11000, 2000),
  minor_breaks = seq(0, 11000, 1000), guide = guide_axis(minor.ticks = TRUE)
) +
  scale_color_manual(values = demo_colors) +
  scale_fill_manual(values = demo_colors) +
  theme_minimal(base_size = 10, base_family = "Helvetica") +  
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = "black"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),                   
    axis.text.y = element_text(size = 10, color = "black"),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10)
  ) +
  labs(
    title = "A",
    y = "Betweenness Centrality"
  ) +
  guides(color = "none", fill = "none")
p_betweenness
# Degree plot
p_degree <- summary_long %>%
  filter(metric == "degree") %>%
  ggplot(aes(x = year, color = node_type, fill = node_type)) +
  geom_ribbon(aes(ymin = smooth_lower, ymax = smooth_upper), alpha = 0.2, color = NA) +
  geom_line(aes(y = smooth_mean, group = node), size = 0.6) +
  geom_point(aes(y = mean, group = node), size = 0.6) +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(
  breaks = seq(0, 50, 10),
  minor_breaks = seq(0, 50, 5),
  guide = guide_axis(minor.ticks = TRUE)) +
  scale_color_manual(values = demo_colors) +
  scale_fill_manual(values = demo_colors) +
  theme_minimal(base_size = 10, base_family = "Helvetica") +  
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = "black"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),                   
    axis.text.y = element_text(size = 10, color = "black"),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10)
  ) +
  labs(
    title = "B",
    x = "Year", 
    y = "Degree Centrality"
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 3)), fill = "none")

p_demo_combined <- (p_betweenness | p_degree) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

windows()
print(p_demo_combined)

showtext_opts(dpi = 300)
mypath1 = "plots/fig3_horizontal_points.tiff"
tiff(file = mypath1, width = 2500, height = 1000, res = 300, pointsize = 10, compression = "lzw")
print(p_demo_combined)
dev.off()

#=============================================================================
#Figure 4: PCA of network correlation matrix
#=============================================================================
library(igraph)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load bootstrapped results
load("bootstrapped_pred_corrs_500_0312_filtered_by_se.saved") #called filtered_results, a list of 500 bootstrap replication
load("yearlist.saved")
ls()
# Variables of interest
track_vars <- c("educ", "age", "race", "partyid", "polviews")

# Helper: parse "varA_varB" into two node names
parse_j <- function(j_string) {
  parts <- strsplit(as.character(j_string), "_")[[1]]
  if (length(parts) == 2) return(data.frame(x = parts[1], y = parts[2]))
  for (i in 1:(length(parts) - 1)) {
    x <- paste(parts[1:i], collapse = "_")
    y <- paste(parts[(i + 1):length(parts)], collapse = "_")
    return(data.frame(x = x, y = y))
  }
}

# Helper: build symmetric correlation matrix from a dataframe for one year
build_corr_matrix <- function(df_year) {
  df_year$corr <- ifelse(is.na(df_year$c_obs), df_year$c_est, df_year$c_obs)
  df_year <- df_year[!is.na(df_year$corr), ]
  if (nrow(df_year) == 0) return(NULL)
  
  j_parsed <- do.call(rbind, lapply(df_year$j, parse_j))
  df_year$x <- j_parsed$x
  df_year$y <- j_parsed$y
  
  nodes <- sort(unique(c(df_year$x, df_year$y)))
  n <- length(nodes)
  mat <- matrix(0, nrow = n, ncol = n, dimnames = list(nodes, nodes))
  diag(mat) <- 1  # self-correlation = 1
  
  for (k in seq_len(nrow(df_year))) {
    mat[df_year$x[k], df_year$y[k]] <- df_year$corr[k]
    mat[df_year$y[k], df_year$x[k]] <- df_year$corr[k]
  }
  mat
}

# For each bootstrap, for each year: build corr matrix, run PCA, extract metrics
cat("Running PCA across", length(filtered_results), "bootstraps x", length(yearlist), "years...\n")

library(parallel)
library(pbapply)
cl <- makeCluster(4, type = "PSOCK")
clusterExport(cl, c("filtered_results", "yearlist", "track_vars", "build_corr_matrix", "parse_j"),
              envir = environment())

pca_all_boots <- pblapply(seq_along(filtered_results), function(b) {
  df <- filtered_results[[b]]
  
  boot_results <- lapply(yearlist, function(yr) {
    df_yr <- df[df$year == yr, ]
    if (nrow(df_yr) == 0) return(NULL)
    
    adj <- build_corr_matrix(df_yr)
    if (is.null(adj) || nrow(adj) < 3) return(NULL)
    
    node_names <- rownames(adj)
    
    # Eigen decomposition
    eigen_result <- tryCatch(eigen(adj), error = function(e) NULL)
    if (is.null(eigen_result)) return(NULL)
    
    eigenvalue1 <- eigen_result$values[1]
    eigenvalue2 <- eigen_result$values[2]
    
    # Variable-PC1 loadings via leave-one-out correlation
    variable_loadings <- sapply(track_vars, function(var) {
      if (!(var %in% node_names)) return(NA_real_)
      var_idx <- which(node_names == var)
      adj_excl <- adj[-var_idx, -var_idx]
      if (nrow(adj_excl) < 2) return(NA_real_)
      eigen_excl <- tryCatch(eigen(adj_excl), error = function(e) NULL)
      if (is.null(eigen_excl)) return(NA_real_)
      pc1_excl <- eigen_excl$vectors[, 1]
      val <- abs(cor(pc1_excl, adj[var_idx, -var_idx], use = "complete.obs"))
      if (is.na(val)) 0 else val
    })
    
    data.frame(
      year = yr, boot_id = b,
      eigenvalue1 = eigenvalue1, eigenvalue2 = eigenvalue2,
      educ = variable_loadings["educ"],
      age = variable_loadings["age"],
      race = variable_loadings["race"],
      partyid = variable_loadings["partyid"],
      polviews = variable_loadings["polviews"],
      row.names = NULL
    )
  })
  
  do.call(rbind, boot_results)
}, cl = cl)

stopCluster(cl)

pca_df_all <- do.call(rbind, pca_all_boots)
cat("Total PCA rows:", nrow(pca_df_all), "\n")
head(pca_df_all)
save(pca_df_all, file = "pca_df_all_500bootstrap.saved")


# Summarize: mean ± 2*SE across bootstraps for each year
pca_summary <- pca_df_all %>%
  group_by(year) %>%
  summarise(
    eigenvalue1_mean = mean(eigenvalue1, na.rm = TRUE),
    eigenvalue1_se   = sd(eigenvalue1, na.rm = TRUE),
    eigenvalue2_mean = mean(eigenvalue2, na.rm = TRUE),
    eigenvalue2_se   = sd(eigenvalue2, na.rm = TRUE),
    educ_mean     = mean(educ, na.rm = TRUE),
    educ_se       = sd(educ, na.rm = TRUE),
    age_mean      = mean(age, na.rm = TRUE),
    age_se        = sd(age, na.rm = TRUE),
    race_mean     = mean(race, na.rm = TRUE),
    race_se       = sd(race, na.rm = TRUE),
    partyid_mean  = mean(partyid, na.rm = TRUE),
    partyid_se    = sd(partyid, na.rm = TRUE),
    polviews_mean = mean(polviews, na.rm = TRUE),
    polviews_se   = sd(polviews, na.rm = TRUE),
    .groups = "drop"
  )

print(pca_summary)
save(pca_summary, file = "pca_summary_500bootstrap.saved")

# Plotting
load("pca_summary_500bootstrap.saved")
head(pca_summary)

font_add("Helvetica", regular = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueRoman.otf", bold = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueBold.otf")
showtext_auto()


loading_long <- pca_summary %>%
  select(year, educ_mean, educ_se, age_mean, age_se, race_mean, race_se,
         partyid_mean, partyid_se, polviews_mean, polviews_se) %>%
  pivot_longer(cols = -year, names_to = c("variable", ".value"),
               names_pattern = "(.+)_(mean|se)") %>%
  mutate(
    var_type = ifelse(variable %in% c("partyid", "polviews"), "Political", "Demographic"),
    variable = case_when(
      variable == "educ" ~ "Education",
      variable == "age" ~ "Age",
      variable == "race" ~ "Race",
      variable == "partyid" ~ "Party",
      variable == "polviews" ~ "Ideology",
      TRUE ~ variable
    ),
    variable = factor(variable, levels = c("Party", "Ideology",
                                           "Education", "Age", "Race"))
  ) %>%
  group_by(variable) %>%
  mutate(
    mean_fit = predict(loess(mean ~ year, span = 0.2, na.action = na.exclude)),
    se_fit   = predict(loess(se   ~ year, span = 0.2, na.action = na.exclude)),
    lo = mean_fit - 2 * se_fit,
    hi = mean_fit + 2 * se_fit
  ) %>%
  ungroup()

p_loadings <- ggplot(loading_long, aes(x = year, y = mean, color = variable, fill = variable)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.12, linewidth = 0, color = NA) +
  geom_point(size = 1) +
  geom_line(data = . %>% filter(!(variable %in% c("Party", "Ideology"))), aes(y = mean_fit), linewidth = 0.6) +
  geom_line(data = . %>% filter(variable %in% c("Party", "Ideology")), aes(y = mean_fit), linewidth = 0.6) +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
  minor_breaks = seq(1970, 2030, 5),
  guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(
  breaks = seq(0, 1, 0.2),
  minor_breaks = seq(0, 1, 0.1), guide = guide_axis(minor.ticks = TRUE)
) +
  scale_color_manual(values = c(
    "Education" = "dark green",
    "Age" = "#ff7928",
    "Race" = "#008dff",
    "Party" = "#d83034",
    "Ideology" = "#000000"
  )) +
  scale_fill_manual(values = c(
    "Education" = "dark green",
    "Age" = "#ff7928",
    "Race" = "#008dff",
    "Party" = "#d83034",
    "Ideology" = "#000000"
  )) +
  labs(
    x = "Year",
    y = expression("PC1 Loadings"),
    color = ""
  ) +
  theme_minimal(base_size = 10, base_family = "Helvetica") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = "black"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    plot.margin = margin(t = 5, r = 10, b = 10, l = 10)
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 3)), fill = "none")


windows()
print(p_loadings)


tiff("plots/fig4.tiff", width = 2000, height = 1000, res = 300, pointsize = 10, compression = "lzw")
print(p_loadings)
dev.off()

# Save data
save(pca_df_all, pca_summary, file = "pca_network_results_bootstrapped.saved")


#=============================================================================
#Figure 5: Correlation of Party, Ideology, Education Over Time
#=============================================================================
# Load bootstrap results (500 replications with filtering by SE)
load("bootstrapped_pred_corrs_500_0312_filtered_by_se.saved")

# Define target node pairs
target_pairs <- c("partyid_polviews", "polviews_partyid", 
                  "partyid_educ", "educ_partyid",
                  "educ_polviews", "polviews_educ")

# Extract correlations for target node pairs across all 500 bootstraps
correlation_results <- do.call(rbind, lapply(1:length(filtered_results), function(boot_idx) {
  df <- filtered_results[[boot_idx]]
  
  # find target pairs
  target_rows <- df[df$j %in% target_pairs, ]
  
  # Use observed c_obs if available, otherwise use estimated c_est
  target_rows$corr <- ifelse(!is.na(target_rows$c_obs), target_rows$c_obs, target_rows$c_est)
  
  target_rows$pair <- apply(target_rows, 1, function(row) {
    nodes <- unlist(strsplit(row["j"], "_"))
    if (length(nodes) == 2) {
      paste(sort(nodes), collapse = "_")
    } else {
      row["j"]
    }
  })
  
  return(data.frame(
    pair = target_rows$pair,
    year = target_rows$year,
    corr = target_rows$corr,
    bootstrap_rep = boot_idx,
    stringsAsFactors = FALSE
  ))
}))

# Calculate mean and SE for each pair-year combination across 500 bootstrap samples
plot_data_summary <- correlation_results %>%
  group_by(pair, year) %>%
  summarise(
    n_bootstrap = n(),
    mean_corr = mean(corr, na.rm = TRUE),
    sd_corr = sd(corr, na.rm = TRUE),
    q025 = mean_corr - 2*sd_corr,#quantile(corr, 0.025, na.rm = TRUE),
    q975 = mean_corr + 2*sd_corr,#quantile(corr, 0.975, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    pair_label = case_when(
      pair == "educ_partyid" ~ "Party-Education",
      pair == "educ_polviews" ~ "Ideology-Education",
      pair == "partyid_polviews" ~ "Party-Ideology",
      TRUE ~ pair
    ),
    pair_label = factor(pair_label, levels = c("Party-Ideology", "Party-Education", "Ideology-Education"))
  )


plot_data_smooth <- plot_data_summary %>%
  mutate(
    lower_bound = q025,
    upper_bound = q975
  ) %>%
  group_by(pair) %>%
  mutate(
    smooth_mean = loess(mean_corr ~ year, span = 0.2, data = cur_data_all())$fitted,
    smooth_lower = loess(lower_bound ~ year, span = 0.2, data = cur_data_all())$fitted,
    smooth_upper = loess(upper_bound ~ year, span = 0.2, data = cur_data_all())$fitted
  ) %>%
  ungroup() %>%
  mutate(
    legend_label = case_when(
      pair_label == "Party-Ideology" ~ "Party and Ideology",
      pair_label == "Party-Education" ~ "Party",
      pair_label == "Ideology-Education" ~ "Ideology",
      TRUE ~ as.character(pair_label)
    ),
    legend_label = factor(legend_label, levels = c("Party and Ideology", " ", "Education and:", "Party", "Ideology"))
  )

# Aesthetics
# Add dummy rows for the spacer and "Education and:" legend header
dummy_spacer <- plot_data_smooth[1, ]
dummy_spacer$smooth_mean <- NA
dummy_spacer$smooth_lower <- NA
dummy_spacer$smooth_upper <- NA
dummy_spacer$legend_label <- factor(" ", levels = c("Party and Ideology", " ", "Education and:", "Party", "Ideology"))

dummy_header <- plot_data_smooth[1, ]
dummy_header$smooth_mean <- NA
dummy_header$smooth_lower <- NA
dummy_header$smooth_upper <- NA
dummy_header$legend_label <- factor("Education and:", levels = c("Party and Ideology", " ", "Education and:", "Party", "Ideology"))

plot_data_smooth <- bind_rows(plot_data_smooth, dummy_spacer, dummy_header)
plot_data_smooth$legend_label <- factor(plot_data_smooth$legend_label,
  levels = c("Party and Ideology", " ", "Education and:", "Party", "Ideology"))


# Create plot
educ_party_pol <- ggplot(plot_data_smooth, aes(x = year, y = smooth_mean, color = legend_label, fill = legend_label)) +
  geom_ribbon(aes(ymin = smooth_lower, ymax = smooth_upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.6) +
  geom_point(aes(y = mean_corr), size = 1) +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), minor_breaks = seq(0, 1, 0.1), guide = guide_axis(minor.ticks = TRUE)) +
  scale_color_manual(values = c(
    "Party and Ideology" = "#000000",
    " " = NA,
    "Education and:" = NA,
    "Party" = "#d83034",
    "Ideology" = "#4ecb8d"
  ), na.value = NA) +
  scale_fill_manual(values = c(
    "Party and Ideology" = "#000000",
    " " = NA,
    "Education and:" = NA,
    "Party" = "#d83034",
    "Ideology" = "#4ecb8d"
  ), na.value = NA) +
  labs(
    title = "A",
    x = "Year",
    y = expression(Correlation~(italic(r))),
    color = "Pair",
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = "black"),
    legend.key.width = unit(0.5, "cm"),
    legend.justification = c(0, 0.5)
  ) +
  guides(
  color = guide_legend(override.aes = list(
    linewidth = c(3, 0, 0, 3, 3),
    alpha = c(1, 0, 0, 1, 1)
  )),
  fill = "none"
)


windows()
print(educ_party_pol)

showtext_opts(dpi = 300)
mypath1 = "plots/fig5.tiff"
tiff(file = mypath1, width = 2200, height = 1000, res = 300, pointsize = 10, compression = "lzw")
print(educ_party_pol)
dev.off()
