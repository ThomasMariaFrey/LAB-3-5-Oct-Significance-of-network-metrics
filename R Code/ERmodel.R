# Set directory
setwd("~/GitHub/LAB-3-5-Oct-Significance-of-network-metrics/")

# Load required libraries
library(igraph)

# Assuming you have already defined the 'languages' vector and 'results' dataframe
# Define the list of languages
languages <- c("Arabic", "Basque", "Catalan", "Chinese", "Czech", 
               "English", "Greek", "Hungarian", "Italian", "Turkish")

results <- results

# Create an empty dataframe to store the mean local clustering coefficients
clustering_results <- data.frame(
  Language = character(0),
  RealNetworkClustering = numeric(0),
  stringsAsFactors = FALSE
)

for (lang in languages) {
  # Compute the local clustering coefficient for each node in the real network
  real_network <- data_list[[lang]]
  real_clustering_values <- transitivity(real_network, type = "local", vids = V(real_network)[degree(real_network) >= 2])
  
  # Compute the mean of the local clustering coefficients, excluding nodes with a degree less than 2
  # -> essentially excluding loops and multiedges
  real_clustering <- mean(real_clustering_values, na.rm = TRUE)
  
  
  
  # Append to clustering_results dataframe
  clustering_results <- rbind(clustering_results, data.frame(
    Language = lang,
    RealNetworkClustering = real_clustering
  ))
}

compute_ERmodels <- function(results, t) {
  list_er_graphs <- list()
  
  for (x in 1:nrow(results)) {
    N = as.numeric(results$N[x])
    E = as.numeric(results$E[x])
    
    graphs_for_lang <- list()
    for (it in 1:t) {
      graph = erdos.renyi.game(N, E, type = "gnm")
      graphs_for_lang[[it]] <- graph
    }
    
    list_er_graphs[[as.character(results$Language[x])]] <- graphs_for_lang
  }
  
  return(list_er_graphs)
}

t = 1
list_er_graphs <- compute_ERmodels(results, t)


er_clustering_results <- list()

for (lang in languages) {
  er_graphs <- list_er_graphs[[lang]]
  clustering_values <- numeric(t)
  
  for (i in 1:t) {
    graph <- er_graphs[[i]]
    clustering <- transitivity(graph, type = "local", vids = V(graph)[degree(graph) >= 2])
    clustering_values[i] <- mean(clustering, na.rm = TRUE)
  }
  
  er_clustering_results[[lang]] <- clustering_values
}

p_values <- numeric(length(languages))
names(p_values) <- languages

for (lang in languages) {
  real_clustering <- clustering_results[clustering_results$Language == lang,]$RealNetworkClustering
  er_clusterings <- er_clustering_results[[lang]]
  p_values[lang] <- sum(er_clusterings >= real_clustering) / t
}

print(p_values)




############ compute p-values


# Number of permutations
num_permutations <- 1000

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


