#Code to turn observed correlations into networks for each year
rm(list=ls())

library(igraph)

#Change the working directory as needed
setwd("Z:/jc3528/OilSpill/CultureNetwork_0312")
getwd()

# Load required data files
load(file = "modelinput_0312.saved")
load("corr_predictions_model_0312.saved") #called e in the file
load("yearlist.saved")


###############################################
for(y in yearlist) {
    print(y)
    filename = paste("corr_predictions_model_0312", y, ".saved", sep="")
    load(file=filename)
  
    #Prepare weighted edgelist
    e$x = r$x[match(e$j, r$j)]
    e$y = r$y[match(e$j, r$j)]

    e$weight = ifelse(is.na(e$c_obs), e$c_est, e$c_obs)
    e$weight[e$weight<0]=0 #bound weights at exactly 0 and 1 for consistency with observed correlations, which are bounded
    e$weight[e$weight>1]=1

    g = graph_from_data_frame(e[, c("x", "y", "weight")], directed=FALSE)

    c = cluster_walktrap(g, weights = E(g)$weight) #community detection 
    V(g)$com = c$membership #save module membership for future use 
  
    save(g, file=paste("full_network_0312", y, ".saved", sep=""))
    print(length(V(g)))
  }


#save all networks as a large list for later analysis
files <- list.files(pattern = "^full_network_0312[0-9]{4}\\.saved$")
files
networks <- list()

for (f in files) {
  # extract year from the filename
  year <- sub("^full_network_0312([0-9]{4})\\.saved$", "\\1", f)
  e <- new.env()
  load(f, envir = e)
  
  obj_name <- ls(e)[1]
  obj <- e[[obj_name]]
 
  if (!inherits(obj, "igraph")) {
    obj <- as.igraph(obj)
  }
  # store in the list, named by year
  networks[[year]] <- obj
}

# `networks` is a list: networks[["2020"]], networks[["2021"]], etc. of igraph objects
save(networks, file = "networks0312.saved")

#=======================================================================================
# There are two ways to filter for statistical significance.
# Method 1: Year-specific pooled thresholds, calculate SE for each correlation using the
# analytical Pearson formula, pool all SE for that year and take the mean, threshold = 2*SE_year, applies a single
# cutoff to all edges in that year
# This is simpler but less precise, use to gain a quick network visualization

for(year in yearlist) {
  year_name <- as.character(year)
  print(year_name)
  corr_file <- paste0("corr_predictions_model_0312", year, ".saved")
  if(file.exists(corr_file)) {
    load(corr_file)  # Loads correlation data
    # Calculate SE directly from this year's data
    if("c_obs" %in% colnames(e)) {
      e$se <- sqrt((1 - e$c_obs^2) / (e$n_obs - 2)) #this is always positive)
      head(e)      
      # Get observed correlations with SE values
      obs_data <- e[!is.na(e$c_obs) & !is.na(e$se), ]
      head(obs_data)
      if(nrow(obs_data) > 0) {
        # Calculate year-specific threshold
        mean_se <- mean(abs(obs_data$se), na.rm = TRUE) # Use absolute SE values
        threshold <- 2 * mean_se
        
        cat("  Year", year, "- Observed correlations:", nrow(obs_data), "\n")
        cat("  Year", year, "- Mean SE:", round(mean_se, 4), "Threshold:", round(threshold, 4), "\n")

        if(year_name %in% names(networks)) {
          net <- networks[[year_name]]
          edge_weights <- E(net)$weight
          keep_edges <- !is.na(edge_weights) & abs(edge_weights) >= threshold
          
          networks[[year_name]] <- delete_edges(net, which(!keep_edges))
          print(summary(E(networks[[year_name]])$weight))
          
          cat("  Original edges:", ecount(net), "Filtered edges:", ecount(networks[[year_name]]), "\n")
        }
      } else {
        cat("  No valid SE data found\n")
      }
    } else {
      cat("  Missing c_obs or se columns\n")
    }
  } else {
    cat("  File not found:", corr_file, "\n")
  }
}

save(networks, file = "networks_0312_filtered_by_se.saved")



summary(E(networks[[1]])$weight)
length(E(networks[[1]]))


# Use Bootstrapped Values:
load("all_thresholds_0312_500.saved")
load("networks0312.saved")
head(all_thresholds)

#=======================================================================================
# Method 2: use edge-specific bootstrap thresholds.
# Each edge pair has edge-specific thresholds calculated from 500 bootstrap samples in Step6_Bootstrapping_part2.5_Filter_by_SE.R

# Load pre-computed thresholds
load("all_thresholds_0312_500.saved")
load("yearlist.saved")

for (year in yearlist) {
  year_name <- as.character(year)
  if (!(year_name %in% names(networks))) next
  
  net <- networks[[year_name]]
  thresholds_year <- all_thresholds[all_thresholds$year == year, ]
  threshold_map <- setNames(thresholds_year$threshold_2sd, thresholds_year$j)
  
  # Filter edges by threshold
  edge_list <- as_edgelist(net, names = TRUE)
  keep_edges <- sapply(seq_len(nrow(edge_list)), function(i) {
    n1 <- edge_list[i, 1]
    n2 <- edge_list[i, 2]
    j1 <- paste0(n1, "_", n2)
    j2 <- paste0(n2, "_", n1)
    thresh <- threshold_map[j1]
    if (is.na(thresh)) thresh <- threshold_map[j2]
    !is.na(thresh) && abs(E(net)$weight[i]) >= thresh
  })
  
  networks[[year_name]] <- delete_edges(net, which(!keep_edges))
  cat("Year", year, "- Original edges:", ecount(net), "Filtered:", ecount(networks[[year_name]]), "\n")
}

save(networks, file = "networks_0312_filtered_by_2sd_thresholds.saved")



summary(E(networks[[1]])$weight)
length(E(networks[[1]]))



#=========================================================
#Visualizations
library(igraph)

# Load files
load("networks_0312_filtered_by_2sd_thresholds.saved")
load("yearlist.saved")

#load tags
node_tags <- read.csv("Z:/jc3528/OilSpill/Sequence_Analysis/node_tags_processed.csv") # Contains issue tags for each survey item
node_label_lookup <- setNames(node_tags$group_tag, node_tags$node)
head(node_label_lookup)

# Visualization
# Process and export networks by year into graphml objects used in Gephi
# For each network, assign node attributes for category and vis_label, and edge attributes for edge_type based on the category of the higher-degree endpoint. Then export as graphml for use in Gephi.
export_network_by_year <- function(year, networks, yearlist, node_tags, persistent_top_nodes, output_dir = ".") {
  idx <- which(yearlist == year)
  cat("Processing network for year:", year, "index:", idx, "\n")
  g <- networks[[idx]]

  # Define node categories and labels
  politic_nodes <- c("partyid", "polviews")
  demo_nodes <- c("region", "age", "race", "educ", "realinc", "prestg10", "size", "sex", "relig")
  
  vis_labels_map <- c(
    "partyid" = "Party",
    "polviews" = "Ideology",
    "race" = "Race",
    "age" = "Age",
    "educ" = "Education"
  )
  
  node_label_lookup <- setNames(node_tags$group_tag, node_tags$node)
  node_names <- V(g)$name
  category_labels <- character(length(node_names))
  vis_labels <- character(length(node_names))
  
  for (i in 1:length(node_names)) {
    node_name <- node_names[i]

    # Assign category attribute
    if (node_name %in% politic_nodes) {
      category_labels[i] <- "politic"
    } else if (node_name %in% demo_nodes) {
      category_labels[i] <- "demog"
    } else if (node_name %in% names(node_label_lookup)) {
      # Get category from node_tags
      category_labels[i] <- node_label_lookup[node_name]
    }

    # Assign vis_label attribute
    if (node_name %in% names(vis_labels_map)) {
      vis_labels[i] <- vis_labels_map[node_name]
    } else {
      vis_labels[i] <- ""
    }
  }

  V(g)$category <- category_labels
  V(g)$vis_label <- vis_labels
  V(g)$betw <- betweenness(g, weights = 1/E(g)$weight)
  
  # Assign edge type based on the category of the higher-degree endpoint
  # Shorten category names for edge labels
  short_cat <- c(
    "demog" = "demo",
    "politic" = "politic",
    "socio-cultural" = "socio",
    "public_policy" = "pub",
    "econ" = "econ"
  )
  
  edge_list <- as_edgelist(g)
  edge_types <- character(nrow(edge_list))
  node_degrees <- degree(g)
  
  for (e_idx in 1:nrow(edge_list)) {
    n1 <- edge_list[e_idx, 1]
    n2 <- edge_list[e_idx, 2]
    
    cat1 <- V(g)$category[match(n1, V(g)$name)]
    cat2 <- V(g)$category[match(n2, V(g)$name)]
    
    # Map to short names
    short1 <- ifelse(cat1 %in% names(short_cat), short_cat[cat1], cat1)
    short2 <- ifelse(cat2 %in% names(short_cat), short_cat[cat2], cat2)
    
    if (short1 == short2) {
      # Same type: just the category name
      edge_types[e_idx] <- short1
    } else {
      # Different types: label by category of the higher-degree node
      deg1 <- node_degrees[n1]
      deg2 <- node_degrees[n2]
      
      if (deg1 > deg2) {
        edge_types[e_idx] <- short1
      } else if (deg2 > deg1) {
        edge_types[e_idx] <- short2
      } else {
        # Equal degree: pick one randomly
        edge_types[e_idx] <- sample(c(short1, short2), 1)
      }
    }
  }

  E(g)$edge_type <- edge_types
  
  output_file <- file.path(output_dir, paste0("network_", year, "_processed.graphml"))
  write_graph(g, output_file, format = "graphml")
  cat("Network has", length(V(g)), "nodes and", length(E(g)), "edges\n")
  
  return(g)
}

#======== usage ========================================================================
# load("networks_0312_filtered_by_se.saved")  # loads 'networks'
# load("yearlist.saved")                       # loads 'yearlist'
# node_tags <- read.csv("Z:/jc3528/OilSpill/Sequence_Analysis/node_tags_processed.csv")
# Export graphml objects for visualization in Gephi

export_network_by_year(1985, networks, yearlist, node_tags, persistent_top_nodes)
export_network_by_year(2024, networks, yearlist, node_tags, persistent_top_nodes)