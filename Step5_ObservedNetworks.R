#Code to turn observed correlations into networks for each year


require(igraph)


rm(list=ls())

#Change the working directory as needed
setwd("Z:/jc3528/OilSpill/CultureNetwork0312")
getwd()


load(file="yearlist.saved")
load(file = "modelinput_0212.saved")
load("corr_predictions_model_02122024.saved") #called e in the file
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
#This is for pearson correlation


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
# Filter networks using all_thresholds
load("all_thresholds_0312_500.saved")
#   j                year threshold_2sd
#   <fct>           <dbl>         <dbl>
# 1 abdefect_age     1972        0.0542
# 2 abdefect_busing  1972        0.0611
# 3 abdefect_cappun  1972        0.0531
# 4 abdefect_colath  1972        0.0522
# 5 abdefect_colcom  1972        0.0517
# 6 abdefect_courts  1972        0.0410
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
load("networks_0312_filtered_by_se.saved")
load("yearlist.saved")


#load tags
node_tags <- read.csv("Z:/jc3528/OilSpill/Sequence_Analysis/node_tags_processed.csv")
node_label_lookup <- setNames(node_tags$group_tag, node_tags$node)
head(node_label_lookup)

# Visualization
# process and export networks by year
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
    "region" = "Region",
    "educ" = "Education",
    "libath" = "Allow Atheist book in library",
    "homosex" = "Accept gay relationships",
    "libcom" = "Allow communist book in library",
    "spkcom" = "Allow communist speaker"
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

export_network_by_year(1996, networks, yearlist, node_tags, persistent_top_nodes)
export_network_by_year(2024, networks, yearlist, node_tags, persistent_top_nodes)



load("networks_0312_filtered_by_se.saved")
head(networks)

#=========================================================
# Generate and save network visualizations for GIF creation
library(igraph)
library(ggraph)
library(ggplot2)

# Load node tags for coloring
node_tags <- read.csv("Z:/jc3528/OilSpill/Sequence_Analysis/node_tags_processed.csv")

# Create color mapping for group_tag
group_colors <- c(
  "socio-cultural" = "#f9e858",
  "econ" = "#008dff",
  "public_policy" = "#4ecb8d",
  "politic" = "#000000"
)

# Add demographic color
group_colors["demog"] <- "#ff7928"

# Create output directory for network images
if (!dir.exists("network_images")) {
  dir.create("network_images")
}

# Create a reference layout once for all years (computed from union of all nodes)
# This is created outside the function to be reused
compute_reference_layout <- function(networks, node_tags) {
  # Collect all unique nodes across all networks
  all_nodes <- unique(unlist(lapply(networks, function(g) V(g)$name)))
  
  # Create a union graph with all nodes and edges from all years
  all_edges <- data.frame()
  for (g in networks) {
    edges <- as_edgelist(g, names = TRUE)
    edge_weights <- E(g)$weight
    edge_df <- data.frame(from = edges[, 1], to = edges[, 2], weight = edge_weights)
    all_edges <- rbind(all_edges, edge_df)
  }
  
  # Aggregate weights by edge (use mean)
  all_edges <- aggregate(weight ~ from + to, data = all_edges, FUN = mean)
  
  # Create the union graph
  g_union <- graph_from_data_frame(all_edges, directed = FALSE, vertices = data.frame(name = all_nodes))
  
  # Compute layout once with a fixed seed
  set.seed(42)
  layout <- layout_with_kk(g_union, weights = E(g_union)$weight)
  
  # Return named coordinates
  coords <- data.frame(node = all_nodes, x = layout[, 1], y = layout[, 2])
  return(coords)
}

# Function to visualize and save network as JPG
save_network_image <- function(g, year, node_tags, group_colors, ref_coords, output_dir = "network_images") {
  
  # Remove edges with weight <= 0 (required for Kamada-Kawai layout)
  g <- delete_edges(g, which(E(g)$weight <= 0.2))
    # Keep only the largest connected component
  comp <- components(g)
  largest_comp <- which.max(comp$csize)
  g <- induced_subgraph(g, which(comp$membership == largest_comp))
  
  # Create a data frame for nodes with their categories
  node_names <- V(g)$name
  node_data <- data.frame(
    node = node_names,
    stringsAsFactors = FALSE
  )
  
  # Map group_tag from node_tags
  node_data$group_tag <- sapply(node_names, function(n) {
    if (n %in% c("partyid", "polviews")) {
      return("politic")
    } else if (n %in% c("region", "age", "race", "educ", "realinc", "prestg10", "size", "sex", "relig")) {
      return("demog")
    } else {
      tag <- node_tags$group_tag[node_tags$node == n]
      if (length(tag) > 0) return(tag[1]) else return(NA)
    }
  })
  
  # Assign colors based on group_tag
  node_data$color <- sapply(node_data$group_tag, function(tag) {
    ifelse(tag %in% names(group_colors), group_colors[tag], "#cccccc")
  })
  
  # Convert igraph to ggraph format
  edges <- as_edgelist(g)
  edges_df <- data.frame(
    from = edges[, 1],
    to = edges[, 2],
    weight = E(g)$weight,
    stringsAsFactors = FALSE
  )
  
  # Get fixed coordinates from reference layout
  fixed_layout <- ref_coords[ref_coords$node %in% node_names, ]
  fixed_layout <- fixed_layout[match(node_names, fixed_layout$node), ]
  
  # Create ggraph visualization with fixed layout for consistency across years
  p <- ggraph(g, layout = fixed_layout[, c("x", "y")]) +
    geom_edge_link(aes(width = weight), alpha = 0.3, color = "gray60") +
    geom_node_point(aes(color = node_data$group_tag[match(name, node_names)]), 
                    size = 5) +
    scale_color_manual(values = group_colors, na.value = "#cccccc") +
    scale_edge_width(range = c(0.5, 3)) +
    theme_void() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    ggtitle(paste("Network", year))
  
  # Save as JPG
  output_file <- file.path(output_dir, paste0("network_", year, ".jpg"))
  ggsave(output_file, plot = p, width = 12, height = 10, dpi = 150, device = "jpeg")
  
  cat("Saved:", output_file, "\n")
  return(output_file)
}

# Generate images for all networks
# Compute reference layout once for consistency across all years
ref_coords <- compute_reference_layout(networks, node_tags)

year_list <- names(networks)
for (year in year_list) {
  tryCatch({
    save_network_image(networks[[year]], year, node_tags, group_colors, ref_coords)
  }, error = function(e) {
    cat("Error processing year", year, ":", e$message, "\n")
  })
}

cat("\nAll network images saved to 'network_images' directory\n")
cat("To create a GIF, use ImageMagick: convert -delay 50 network_images/network_*.jpg network_animation.gif\n")
