# Set directory
setwd("~/GitHub/LAB-3-5-Oct-Significance-of-network-metrics/")

# Load required libraries
library(igraph)

# Assuming you have already defined the 'languages' vector and 'results' dataframe
# Define the list of languages
languages <- c("Arabic", "Basque", "Catalan", "Chinese", "Czech", 
               "English", "Greek", "Hungarian", "Italian", "Turkish")

results <- results

# Create an empty list to store the ER graphs
er_graphs <- list()

for (lang in languages) {
  # Get the number of vertices and edges for the language from the results dataframe
  num_vertices <- results[results$Language == lang,]$N
  num_edges <- results[results$Language == lang,]$E
  
  # Generate an Erdős-Rényi graph with the given number of vertices and edges
  er_graph <- sample_gnm(n = num_vertices, m = num_edges, directed = TRUE, loops = FALSE)
  
  # Store the ER graph in the list with the language name as the key
  er_graphs[[lang]] <- er_graph
}

########### check for isolated nodes
for (lang in languages) {
  real_network <- data_list[[lang]]
  er_graph <- er_graphs[[lang]]
  
  real_isolated_nodes <- sum(degree(real_network) < 2)
  er_isolated_nodes <- sum(degree(er_graph) < 2)
  
  cat("For", lang, "- Real Network Isolated Nodes:", real_isolated_nodes, ", ER Graph Isolated Nodes:", er_isolated_nodes, "\n")
}
###########


# Create an empty dataframe to store the mean local clustering coefficients
clustering_results <- data.frame(
  Language = character(0),
  RealNetworkClustering = numeric(0),
  ERGraphClustering = numeric(0),
  stringsAsFactors = FALSE
)

for (lang in languages) {
  # Compute the local clustering coefficient for each node in the real network
  real_network <- data_list[[lang]]
  real_clustering_values <- transitivity(real_network, type = "local", vids = V(real_network)[degree(real_network) >= 2])
  
  # Compute the mean of the local clustering coefficients, excluding nodes with a degree less than 2
  # -> essentially excluding loops and multiedges
  real_clustering <- mean(real_clustering_values, na.rm = TRUE)
  
  # Repeat the same process for the Erdős-Rényi graph
  er_graph <- er_graphs[[lang]]
  er_clustering_values <- transitivity(er_graph, type = "local", vids = V(er_graph)[degree(er_graph) >= 2])
  er_clustering <- mean(er_clustering_values, na.rm = TRUE)
  
  # Append to clustering_results dataframe
  clustering_results <- rbind(clustering_results, data.frame(
    Language = lang,
    RealNetworkClustering = real_clustering,
    ERGraphClustering = er_clustering
  ))
}


############ compute p-values


# Number of permutations
num_permutations <- 100

# Create an empty dataframe to store the p-values
p_values_results <- data.frame(
  Language = character(0),
  PValue = numeric(0),
  stringsAsFactors = FALSE
)

for (lang in languages) {
  real_network <- data_list[[lang]]
  real_clustering <- clustering_results[clustering_results$Language == lang,]$RealNetworkClustering
  
  # Compute the clustering coefficient for permuted networks
  permuted_clusterings <- numeric(num_permutations)
  for (i in 1:num_permutations) {
    # Randomly rewire the edges while keeping the node degrees fixed
    permuted_network <- rewire(real_network, keeping_degseq())
    permuted_clustering_values <- transitivity(permuted_network, type = "local", vids = V(permuted_network)[degree(permuted_network) >= 2])
    permuted_clusterings[i] <- mean(permuted_clustering_values, na.rm = TRUE)
  }
  
  # Compute the p-value
  p_value <- sum(permuted_clusterings >= real_clustering) / num_permutations
  
  # Append to p_values_results dataframe
  p_values_results <- rbind(p_values_results, data.frame(Language = lang, PValue = p_value))
}

print(p_values_results)

# Using one of the larger networks as an example
real_network <- data_list[["English"]] # or any other large network in your data

# Time a single permutation
system.time({
  permuted_network <- rewire(real_network, keeping_degseq())
  permuted_clustering_values <- transitivity(permuted_network, type = "local", vids = V(permuted_network)[degree(permuted_network) >= 2])
  mean(permuted_clustering_values, na.rm = TRUE)
})


