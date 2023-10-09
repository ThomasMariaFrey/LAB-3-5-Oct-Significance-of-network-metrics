# Set directory
setwd("~/GitHub/LAB-3-5-Oct-Significance-of-network-metrics/")

# Load required libraries
library(xtable)
library(data.table)

# Define the list of languages
languages <- c("Arabic", "Basque", "Catalan", "Chinese", "Czech", 
               "English", "Greek", "Hungarian", "Italian", "Turkish")

# Create an empty list to store the adjacency lists
data_list <- list()

# Create an empty dataframe to store information about different degree sequences
results <- data.frame(Language = character(0), 
                      N = numeric(0),
                      E = numeric(0),
                      MeanDegree = numeric(0),
                      Density = numeric(0),
                      stringsAsFactors = FALSE)

for (lang in languages) {
  file_name <- paste0("data/", lang, "_syntactic_dependency_network.txt")
  
  # Read the entire file into a data.table, filtering out loops
  edges <- fread(file_name, skip = 1, header = FALSE, quote = "")
  
  # Create adjacency list using data.table
  adj_list <- edges[, .(V2 = list(V2)), by = V1]
  
  # Store the adjacency list
  assign(paste0(tolower(lang), "_adj_list"), adj_list, envir = .GlobalEnv)
  data_list[[tolower(lang)]] <- adj_list
  
  # Get network metrics
  num_vertices <- length(unique(c(edges$V1, edges$V2)))
  num_edges <- nrow(edges)
  mean_degree <- 2 * num_edges / num_vertices
  network_density <- 2 * num_edges / (num_vertices * (num_vertices - 1))
  
  # Append to results dataframe
  results <- rbind(results, data.frame(Language = lang, 
                                       N = num_vertices,
                                       E = num_edges,
                                       MeanDegree = mean_degree,
                                       Density = network_density))
}

# Export the table for LaTeX
colnames(results) <- c("Language", "N", "E", "<k>", "δ")
latex_table <- xtable(results)
output_file <- "results_table_1.tex"
print(latex_table, type = "latex", file = output_file, floating = FALSE)







