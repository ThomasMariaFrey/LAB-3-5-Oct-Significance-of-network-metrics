# Set directory
setwd("~/GitHub/LAB-3-5-Oct-Significance-of-network-metrics/")

# Load required libraries
library(xtable)
library(data.table)
library(igraph)  # Ensure you load the igraph library

# Define the list of languages
languages <- c("Arabic", "Basque", "Catalan", "Chinese", "Czech", 
               "English", "Greek", "Hungarian", "Italian", "Turkish")

# Create an empty list to store the graph objects
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
  
  # Read the entire file into a data.table
  edges <- fread(file_name, skip = 1, header = FALSE, quote = "", na.strings = "")
  
  # Create an igraph object from the edge list
  graph_obj <- graph_from_data_frame(edges, directed = TRUE)
  
  # Store the graph object
  data_list[[lang]] <- graph_obj
  
  # Get network metrics
  num_vertices <- vcount(graph_obj)
  num_edges <- ecount(graph_obj)
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




# Check rows with NA values for each language 
# -> Czech and Turkish language data contains "NA" as normal string which should not be substituted or removed!
sample_file <- "data/Turkish_syntactic_dependency_network.txt"
sample_data <- fread(sample_file, skip = 1, header = FALSE, quote = "")
print(sample_data[is.na(V1) | is.na(V2), ])






