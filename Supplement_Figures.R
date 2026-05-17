

rm(list=ls())
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")


# Load libraries
library(pbapply)
library(parallel)
library(dplyr)
library(ggplot2)
library(showtext)
library(scales)

# ============================================================================
# Fig. S1  Analysis for degree of political polarization change over time
# ============================================================================
library(haven)
node_tags <- read.csv("Z:/jc3528/OilSpill/Sequence_Analysis/node_tags_processed.csv")
d <- read_dta("Z:/jc3528/OilSpill/Data/GSS_Recoded2024_0204_withdemo_logtransformed.dta")
d <- as.data.frame(lapply(d, as.vector))  # Strip all haven attributes
d <- data.frame(lapply(d, as.numeric), stringsAsFactors = FALSE) 

d <- d %>% select(-class)


#only keep GSS variables we need
load(file="Z:/jc3528/OilSpill/CultureNetwork_0312/full_network_03122024.saved")

V(g)
nodes = V(g)$name
length(nodes)
print(nodes)
class(nodes)
d = d[, colnames(d)=="year"|colnames(d) %in% nodes] #keep only relevant nodes
#d <- data.frame(lapply(d, as.numeric))
str(d)
length(d)

# Normalize all SD-relevant variables to the maximum possible SD scale.
# A variable rescaled to [0, 1] has a maximum SD of 0.5, so multiplying by 2
# expresses dispersion as a proportion of the maximum possible SD.
normalize_to_max_sd <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) {
    return(rep(NA_real_, length(x)))
  }
  x_scaled <- (x - rng[1]) / diff(rng)
  1 * x_scaled
}

# Calculate SD for each individual issue for each year, rank by variance of SD over time, plot top 10
# All issue nodes (exclude year, partyid, polviews, demo vars)
all_issue_vars <- nodes[nodes %in% colnames(d) & !nodes %in% c("year", "educ", "age", "realinc", "race", "prestg10", "region", "relig", "sex", "size")]

# Normalize each variable to the maximum possible SD scale before computing SD over time.
d_normalized <- d %>%
  mutate(across(all_of(all_issue_vars), normalize_to_max_sd))

# Bootstrap resampling: 500 iterations
n_bootstrap <- 500
set.seed(36)

# Store bootstrap results for each variable by year
bootstrap_results <- vector("list", n_bootstrap)

cat("Running", n_bootstrap, "bootstrap resamples...\n")

for (b in 1:n_bootstrap) {
  if (b %% 50 == 0) cat("Bootstrap iteration:", b, "\n")
  
  # Resample respondents with replacement for each year
  d_boot <- d_normalized %>%
    group_by(year) %>%
    slice_sample(prop = 1, replace = TRUE) %>%
    ungroup()
  
  # Calculate SD for each variable by year in this bootstrap sample
  node_sd_by_year_b <- d_boot %>%
    select(year, all_of(all_issue_vars)) %>%
    group_by(year) %>%
    summarise(across(all_of(all_issue_vars), ~sd(., na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(-year, names_to = "node", values_to = "sd_val")
  
  # Join with node_tags and aggregate by group
  node_sd_by_group_b <- node_sd_by_year_b %>%
    left_join(node_tags %>% select(node, group_tag), by = "node") %>%
    filter(!is.na(group_tag) & group_tag != "politic") %>%
    group_by(year, group_tag) %>%
    summarise(
      avg_sd = mean(sd_val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      group_tag = case_when(
        group_tag == "socio-cultural" ~ "Socio-cultural",
        group_tag == "econ" ~ "Economic Policy",
        group_tag == "public_policy" ~ "Public Affairs",
        TRUE ~ group_tag
      ),
      bootstrap_id = b
    ) %>%
    rename(label = group_tag, value = avg_sd)
  
  # Calculate SD for polviews and partyid in this bootstrap
  polviews_sd_b <- d_boot %>%
    select(year, polviews) %>%
    group_by(year) %>%
    summarise(sd_val = sd(polviews, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(sd_val)) %>%
    mutate(label = "Ideology", value = sd_val, bootstrap_id = b) %>%
    select(year, label, value, bootstrap_id)
  
  partyid_sd_b <- d_boot %>%
    select(year, partyid) %>%
    group_by(year) %>%
    summarise(sd_val = sd(partyid, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(sd_val)) %>%
    mutate(label = "Party", value = sd_val, bootstrap_id = b) %>%
    select(year, label, value, bootstrap_id)
  
  # Combine all data for this bootstrap
  bootstrap_results[[b]] <- bind_rows(
    node_sd_by_group_b,
    polviews_sd_b,
    partyid_sd_b
  )
}

# Combine all bootstrap results
all_bootstrap_sd <- bind_rows(bootstrap_results)

# Calculate mean and SE (SD of bootstrap distribution) for each year/item-pair combination
node_sd_combined <- all_bootstrap_sd %>%
  filter(!is.na(label) & !is.na(value)) %>%
  group_by(year, label) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(label = factor(label, levels = c("Socio-cultural", "Economic Policy", "Public Affairs", "Party", "Ideology")))

# Apply LOESS smoothing for each label
node_sd_combined <- node_sd_combined %>%
  group_by(label) %>%
  mutate(
    mean_smooth = predict(loess(mean ~ year, span = 0.2, na.action = na.exclude)),
    se_smooth = predict(loess(se ~ year, span = 0.2, na.action = na.exclude))
  ) %>%
  ungroup()

# Plot average SD by group_tag over time, including polviews and partyid
p_sd_by_group <- ggplot(node_sd_combined, aes(x = year, color = label, fill = label)) +
  geom_ribbon(aes(ymin = mean_smooth - 2 * se_smooth, ymax = mean_smooth + 2 * se_smooth), alpha = 0.12, linewidth = 0, color = NA) +
  geom_line(data = node_sd_combined %>% filter(label %in% c("Socio-cultural", "Economic Policy", "Public Affairs")), 
            aes(y = mean_smooth), linewidth = 0.6) +
  geom_line(data = node_sd_combined %>% filter(label %in% c("Party", "Ideology")), 
            aes(y = mean_smooth), linewidth = 0.6) +
  geom_point(data = node_sd_combined, 
             aes(y = mean), size = 1) +
  scale_y_continuous(
    breaks = seq(0.1, 0.55, 0.1),
    minor_breaks = seq(0.1, 0.55, 0.05),
    guide = guide_axis(minor.ticks = TRUE),
    limits = c(0.1, 0.55)) +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  scale_color_manual(values = c(
    "Socio-cultural" = "#f9e858",
    "Economic Policy" = "#008dff",
    "Public Affairs" = "#4ecb8d",
    "Party" = "#d83034",
    "Ideology" = "#000000"
  ), na.value = NA) +
  scale_fill_manual(values = c(
    "Socio-cultural" = "#f9e858",
    "Economic Policy" = "#008dff",
    "Public Affairs" = "#4ecb8d",
    "Party" = "#d83034",
    "Ideology" = "#000000"
  ), na.value = NA) +
  labs(title = "Extremism",
       x = "Year", 
       y = expression(Extremism~(sigma)),
       color = "Domain/Variable") +
  theme_minimal(base_size = 10, base_family = "Helvetica") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    legend.position = "bottom",
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = "black")
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 3), nrow = 2), fill = "none")

windows()
print(p_sd_by_group)

showtext_opts(dpi = 300)
mypath_sd_group = "plots/figs1.tiff"
tiff(file = mypath_sd_group, width = 1500, height = 1300, res = 300, pointsize = 10, compression = "lzw")
print(p_sd_by_group)
dev.off()
# ====================================================================================================
# Fig S2 Domain correlation with Demographics over time (using bootstrapped results)
# ====================================================================================================
rm(list = ls())
load("Z:/jc3528/OilSpill/CultureNetwork_0312/bootstrapped_pred_corrs_500_0312_filtered_by_se.saved")
node_tags <- read.csv("Z:/jc3528/OilSpill/Sequence_Analysis/node_tags_processed.csv")

head(node_tags)

unique(node_tags$module2)

results <- filtered_results
head(results[[1]])
# Define demographic nodes and political nodes
demo_nodes <- c("educ", "race", "age")
party_ideology <- c("partyid", "polviews")

# extract y and x columns form j column in results[[1]]. j represents each belief pairs
parse_j <- function(j_string) {
  parts <- strsplit(as.character(j_string), "_")[[1]]
  if (length(parts) == 2) {
    return(data.frame(x = parts[1], y = parts[2]))
  } else {
    # Handle cases with multiple underscores by trying different splits
    for (i in 1:(length(parts)-1)) {
      x <- paste(parts[1:i], collapse = "_")
      y <- paste(parts[(i+1):length(parts)], collapse = "_")
      return(data.frame(x = x, y = y))
    }
  }
}

# # Process each bootstrap to calculate domain correlations with demographics
calc_domain_demo_corrs <- function(df, node_tags, demo_nodes) {
  # Parse j into x and y columns
  j_parsed <- do.call(rbind, lapply(df$j, parse_j))
  df$x <- j_parsed$x
  df$y <- j_parsed$y
  
  # Use c_obs if available, else c_est
  df$corr <- ifelse(is.na(df$c_obs), df$c_est, df$c_obs)
  
  # Filter for edges where one node is demographic
  demo_edges <- df %>%
    filter((x %in% demo_nodes | y %in% demo_nodes) & 
           !(x %in% demo_nodes & y %in% demo_nodes)) %>%  # Exclude demo-demo edges
    mutate(other_var = ifelse(x %in% demo_nodes, y, x)) %>%
    left_join(node_tags %>% select(node, group_tag), by = c("other_var" = "node")) %>%
    filter(!is.na(group_tag) & !is.na(corr)) %>%
    group_by(year, group_tag) %>%
    summarise(avg_rho = mean(abs(corr), na.rm = TRUE), .groups = "drop")
  
  demo_edges
}


# Domain correlation with both partyid and ideology
calc_domain_political_corrs <- function(df, node_tags, demo_nodes) {
  # Parse j into x and y columns once
  j_parsed <- do.call(rbind, lapply(df$j, parse_j))
  df$x <- j_parsed$x
  df$y <- j_parsed$y
  df$corr <- ifelse(is.na(df$c_obs), df$c_est, df$c_obs)
  
  # calculate ideology correlations
  ideology_edges <- df %>%
    filter((x == "polviews" | y == "polviews") & 
           !(x %in% demo_nodes | y %in% demo_nodes)) %>%
    mutate(other_var = ifelse(x == "polviews", y, x)) %>%
    filter(other_var != "partyid") %>%
    left_join(node_tags %>% select(node, group_tag), by = c("other_var" = "node")) %>%
    filter(!is.na(group_tag) & !is.na(corr)) %>%
    group_by(year, group_tag) %>%
    summarise(
      avg_rho = mean(abs(corr), na.rm = TRUE), 
      se_rho = sd(abs(corr), na.rm = TRUE),
      n_pairs = n(),
      .groups = "drop"
    ) %>%
    mutate(political_var = "polviews")
  
  # calculate party correlations
  partyid_edges <- df %>%
    filter((x == "partyid" | y == "partyid") & 
           !(x %in% demo_nodes | y %in% demo_nodes)) %>%
    mutate(other_var = ifelse(x == "partyid", y, x)) %>%
    filter(other_var != "polviews") %>%
    left_join(node_tags %>% select(node, group_tag), by = c("other_var" = "node")) %>%
    filter(!is.na(group_tag) & !is.na(corr)) %>%
    group_by(year, group_tag) %>%
    summarise(
      avg_rho = mean(abs(corr), na.rm = TRUE), 
      se_rho = sd(abs(corr), na.rm = TRUE),
      n_pairs = n(),
      .groups = "drop"
    ) %>%
    mutate(political_var = "partyid")
  
  # Combine and return both
  rbind(ideology_edges, partyid_edges)
}


# Set up parallel cluster (use all available cores minus 1)
n_cores <- 4
cl <- makeCluster(n_cores, type = "PSOCK")

# Export necessary objects to cluster
clusterExport(cl, c("calc_domain_political_corrs", "node_tags", "demo_nodes", "parse_j", "results"), 
              envir = environment())
clusterEvalQ(cl, library(dplyr))

# Run parallel processing
all_domain_political_corrs <- pblapply(seq_along(results), function(i) {
  calc_domain_political_corrs(results[[i]], node_tags, demo_nodes) %>% mutate(boot_id = i)
}, cl = cl) |> bind_rows()

# Stop cluster
stopCluster(cl)

# Split into separate datasets for backwards compatibility
all_domain_ideology_corrs <- all_domain_political_corrs %>% filter(political_var == "polviews") %>% select(-political_var)
all_domain_partyid_corrs <- all_domain_political_corrs %>% filter(political_var == "partyid") %>% select(-political_var)

save(all_domain_ideology_corrs, file = "all_domain_ideology_corrs_500bootstrap.saved")
save(all_domain_partyid_corrs, file = "all_domain_partyid_corrs_500bootstrap.saved")
str(all_domain_ideology_corrs)


# Load to plot
load("all_domain_ideology_corrs_500bootstrap.saved")
load("all_domain_partyid_corrs_500bootstrap.saved")
head(all_domain_ideology_corrs)
nrow(all_domain_ideology_corrs)

font_add("Helvetica", regular = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueRoman.otf", bold = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueBold.otf")
showtext_auto()


length(unique(all_domain_ideology_corrs$boot_id)) 

# Summarize with mean and SE across bootstraps for ideology
domain_ideology_summary <- all_domain_ideology_corrs %>%
  group_by(year, group_tag) %>%
  summarise(
    avg_rho_se = sd(avg_rho, na.rm = TRUE),
    avg_rho    = mean(avg_rho, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(group_tag != "politic") %>%
  mutate(group_tag = case_when(
    group_tag == "socio-cultural" ~ "Socio-cultural",
    group_tag == "econ" ~ "Economic Policy",
    group_tag == "public_policy" ~ "Public Affairs",
    TRUE ~ group_tag
  )) %>%
  group_by(group_tag) %>%
  mutate(
    avg_rho_fit = tryCatch(predict(loess(avg_rho ~ year, span = 0.2, na.action = na.exclude)), error = function(e) avg_rho),
    se_rho_fit  = tryCatch(predict(loess(avg_rho_se ~ year, span = 0.2, na.action = na.exclude)), error = function(e) avg_rho_se)
  ) %>%
  ungroup() %>%
  mutate(political_var = "Ideology")

head(domain_ideology_summary)

# Summarize with mean and SE across bootstraps for partyid
domain_partyid_summary <- all_domain_partyid_corrs %>%
  group_by(year, group_tag) %>%
  summarise(
    avg_rho_se = sd(avg_rho, na.rm = TRUE),
    avg_rho    = mean(avg_rho, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(group_tag != "politic") %>%
  mutate(group_tag = case_when(
    group_tag == "socio-cultural" ~ "Socio-cultural",
    group_tag == "econ" ~ "Economic Policy",
    group_tag == "public_policy" ~ "Public Affairs",
    TRUE ~ group_tag
  )) %>%
  group_by(group_tag) %>%
  mutate(
    avg_rho_fit = tryCatch(predict(loess(avg_rho ~ year, span = 0.2, na.action = na.exclude)), error = function(e) avg_rho),
    se_rho_fit  = tryCatch(predict(loess(avg_rho_se ~ year, span = 0.2, na.action = na.exclude)), error = function(e) avg_rho_se)
  ) %>%
  ungroup() %>%
  mutate(political_var = "Party")

# Combine both for faceted plotting
domain_political_combined <- rbind(domain_ideology_summary, domain_partyid_summary)

head(domain_political_combined)

# Create faceted plot with Ideology and Party as panels
domain_political_plot <- ggplot(domain_political_combined, aes(x = year, color = group_tag, fill = group_tag)) +
  geom_ribbon(aes(ymin = avg_rho_fit - 2 * se_rho_fit, ymax = avg_rho_fit + 2 * se_rho_fit), alpha = 0.2, linewidth = 0, color = NA) +
  geom_line(aes(y = avg_rho_fit), linewidth = 0.6) +
  geom_point(aes(y = avg_rho), size = 1) +
  facet_wrap(~ political_var, ncol = 2) +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(
    breaks = seq(0.05, 0.3, 0.05),
    minor_breaks = seq(0.05, 0.3, 0.025),
    guide = guide_axis(minor.ticks = TRUE)) +
  labs(x = "Year", y = expression(Correlation~"("*italic(r)*")"), 
       color = "Domain") +
  scale_color_manual(values = c(
    "Socio-cultural" = "#f9e858",
    "Economic Policy" = "#008dff",
    "Public Affairs" = "#4ecb8d"
  )) +
  scale_fill_manual(values = c(
    "Socio-cultural" = "#f9e858",
    "Economic Policy" = "#008dff",
    "Public Affairs" = "#4ecb8d"
  )) +
  theme_minimal(base_size = 16, base_family = "Helvetica") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = "black"),
    strip.text = element_text(size = 10, color = "black")
  ) +
  guides(
  color = guide_legend(override.aes = list(linewidth = 3)),
  fill = "none"
)

windows()
domain_political_plot

showtext_opts(dpi = 300)
mypath3 = "plots/figs2.tiff"
tiff(file = mypath3, width = 2200, height = 1100, pointsize = 10, res = 300, compression = "lzw")
print(domain_political_plot)
dev.off()


#================================================
# Figure S3. Plot for centrality of Politics and all Demographics
#================================================
# Centrality Timetrend Plots
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")


load("bootstrap_summary_stats_500_0312.RData")
load("node_metrics_list_500_0312.RData")

summary(node_summary$betweenness_mean)
head(node_metrics_list[[1]], 2)
head(node_summary, 2)

font_add("Helvetica", regular = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueRoman.otf", bold = "Z:/jc3528/OilSpill/Fonts/HelveticaNeueBold.otf")
showtext_auto()

# Define nodes to plot
nodes_to_plot <- c("educ", "age", "race", "polviews", "partyid", "sex", "realinc", "prestg10", "region", "relig", "size")

node_summary <- node_summary %>%
  mutate(
    node_type = case_when(
      node == "partyid" ~ "Party",
      node == "polviews" ~ "Ideology", 
      node == "educ" ~ "Education",
      node == "age" ~ "Age",
      node == "race" ~ "Race",
      node == "sex" ~ "Sex",
      node == "realinc" ~ "Income",
      node == "prestg10" ~ "Occupation Prestige",
      node == "region" ~ "Region",
      node == "relig" ~ "Religion",
      node == "size" ~ "Size",
      TRUE ~ "Other Issues"
    ),
    node_type = factor(node_type, levels = c(
      "Party", "Ideology", "Education", "Age", "Race", "Sex", "Income", "Occupation Prestige", "Region", "Religion", "Size"
    ))
  )

# Color palette
demo_colors <- c(
  "Party" = "#d83034",
  "Ideology" = "#000000",
  "Education" = "dark green",
  "Age" = "#ff7928",
  "Race" = "#008dff",
  "Sex" = "#ff73b6",
  "Income" = "gray50",
  "Occupation Prestige" = "#c701ff",
  "Region" = "#4ecb8d",
  "Religion" = "blue",
  "Size" = "#bebe4b"
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
  geom_line(data = . %>% filter(!(node_type %in% c("Party", "Ideology"))), aes(y = smooth_mean, group = node), size = 0.8) +
  geom_line(data = . %>% filter(node_type %in% c("Party", "Ideology")), aes(y = smooth_mean, group = node), size = 0.8) +
  scale_color_manual(values = demo_colors) +
  scale_fill_manual(values = demo_colors) +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(
  breaks = seq(0, 11000, 2500),
  minor_breaks = seq(0, 11000, 1250), guide = guide_axis(minor.ticks = TRUE)
) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +  
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
    title = "A",
    y = "Betweenness Centrality"
  ) +
  guides(color = "none", fill = "none")

# Degree plot
p_degree <- summary_long %>%
  filter(metric == "degree") %>%
  ggplot(aes(x = year, color = node_type, fill = node_type)) +
  geom_ribbon(aes(ymin = smooth_lower, ymax = smooth_upper), alpha = 0.2, color = NA) +
  geom_line(data = . %>% filter(!(node_type %in% c("Party", "Ideology"))), aes(y = smooth_mean, group = node), size = 0.8) +
  geom_line(data = . %>% filter(node_type %in% c("Party", "Ideology")), aes(y = smooth_mean, group = node), size = 0.8) +
  scale_color_manual(values = demo_colors) +
  scale_fill_manual(values = demo_colors) +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(
  breaks = seq(0, 50, 10),
  minor_breaks = seq(0, 50, 5),
  guide = guide_axis(minor.ticks = TRUE)
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +  
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

p_demo_combined_all <- (p_betweenness / p_degree) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

windows()
print(p_demo_combined_all)

showtext_opts(dpi = 300)
mypath1 = "plots/figs3.tiff"
tiff(file = mypath1, width = 1800, height = 1500, res = 300, pointsize = 10, compression = "lzw")
print(p_demo_combined_all)
dev.off()



#============================================================================
# Fig. S4 PC1 Eigenvalue over time
#============================================================================
load("pca_summary_500bootstrap.saved") #load pca_sumary

# Fig S4 Eigenvalues over time
eigen_long <- pca_summary %>%
  select(year, starts_with("eigenvalue")) %>%
  pivot_longer(cols = -year, names_to = c("pc", ".value"),
               names_pattern = "(eigenvalue[12])_(.*)") %>%
  mutate(pc = ifelse(pc == "eigenvalue1", "PC1", "PC2")) %>%
  group_by(pc) %>%
  mutate(
    mean_fit = predict(loess(mean ~ year, span = 0.2, na.action = na.exclude)),
    se_fit   = predict(loess(se   ~ year, span = 0.2, na.action = na.exclude)),
    lo = mean_fit - 2 * se_fit,
    hi = mean_fit + 2 * se_fit
  ) %>%
  ungroup()

p1 <- ggplot(eigen_long, aes(x = year, y = mean, color = pc, fill = pc)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, linewidth = 0, color = NA) +
  geom_point(size = 1, alpha = 0.6) +
  geom_line(aes(y = mean_fit), linewidth = 0.6) +
  labs(x = "Year", y = "Eigenvalue", color = "") +
  scale_x_continuous(
    breaks = seq(1970, 2030, 10),
    minor_breaks = seq(1970, 2030, 5),
    guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_continuous(
    breaks = seq(5, 25, 5),
    minor_breaks = seq(0, 30, 2.5),
    guide = guide_axis(minor.ticks = TRUE)) +
  scale_color_manual(values = c("PC1" = "#d83034", "PC2" = "#000000")) +
  scale_fill_manual(values = c("PC1" = "#d83034", "PC2" = "#000000")) +
  theme_minimal(base_size = 10, base_family = "Helvetica") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "bottom",
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
print(p1)

# Save plots
tiff("plots/figs4.tiff", width = 1500, height = 1200, res = 300, pointsize = 14, compression = "lzw")
print(p1)
dev.off()


