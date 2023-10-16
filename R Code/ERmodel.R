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

#### -- ER graphs --

# Define a function to compute Erdös-Rényi (ER) models for the real networks.
compute_ERmodels <- function(results, num_ergraphs) {
  
  # Create an empty list to store ER graphs for each language.
  list_er_graphs <- list()
  
  # Loop over each language.
  for (x in 1:nrow(results)) {
    
    # Get the number of nodes and edges from the results dataframe.
    N = as.numeric(results$N[x])
    E = as.numeric(results$E[x])
    
    # Create an empty list to store the generated ER graphs for the current language.
    graphs_for_lang <- list()
    
    # Generate num_ergraphs ER graphs for the current language.
    for (it in 1:num_ergraphs) {
      
      # Generate an ER graph with N nodes and E edges.
      graph = erdos.renyi.game(N, E, type = "gnm")
      
      # Append the generated graph to the list.
      graphs_for_lang[[it]] <- graph
    }
    
    # Append the list of generated graphs for the current language to the main list.
    list_er_graphs[[as.character(results$Language[x])]] <- graphs_for_lang
  }
  
  # Return the list containing ER graphs for each language.
  return(list_er_graphs)
}

# Number of ER models to generate for each language.
# -> Monte Carlo procedure
num_ergraphs = 15

# Call the function to generate the ER models.
list_er_graphs <- compute_ERmodels(results, num_ergraphs)



# Create an empty list to store clustering coefficient values for the ER graphs.
er_clustering_results <- list()

# Loop over each language.
for (lang in languages) {
  
  # Get the ER graphs for the current language.
  er_graphs <- list_er_graphs[[lang]]
  
  # Create a vector to store clustering coefficient values.
  clustering_values <- numeric(t)
  
  # Compute clustering coefficients for each ER graph of the current language.
  for (i in 1:t) {
    
    # Get the current ER graph.
    graph <- er_graphs[[i]]
    
    # Compute the local clustering coefficient for each node in the graph.
    clustering <- transitivity(graph, type = "local", vids = V(graph)[degree(graph) >= 2])
    
    # Store the mean clustering coefficient value.
    clustering_values[i] <- mean(clustering, na.rm = TRUE)
  }
  
  # Store the clustering coefficient values for the current language in the main list.
  er_clustering_results[[lang]] <- clustering_values
}

## -- p values for ER models --

# Create a vector to store p-values for each language.
p_values <- numeric(length(languages))
names(p_values) <- languages

# Loop over each language to compute the p-values.
for (lang in languages) {
  
  # Get the real network's clustering coefficient for the current language.
  real_clustering <- clustering_results[clustering_results$Language == lang,]$RealNetworkClustering
  
  # Get the ER graphs' clustering coefficient values for the current language.
  er_clusterings <- er_clustering_results[[lang]]
  
  # Compute the p-value for the current language.
  p_values[lang] <- sum(er_clusterings >= real_clustering) / num_ergraphs
}

# Print the computed p-values.
print(p_values)




## -- analytical way of calculating the p-values for the ER graphs --


# Function to calculate the expected clustering coefficient for an ER graph
expected_clustering <- function(p) {
  return(p)
}

# Function to calculate the variance of the clustering coefficient for an ER graph
variance_clustering <- function(N, p) {
  return(p * (1 - p) / N + p * (1 - 3 * p + p^2) / N^2)
}


# Function to calculate the Z-score
z_score <- function(C, C_ER, variance_C_ER) {
  return((C - C_ER) / sqrt(variance_C_ER))
}

# Function to calculate the p-value based on the Z-score
# Assuming a two-tailed test
p_value_analytical <- function(Z) {
  return(2 * (1 - pnorm(abs(Z))))
}

####################################
## -- Analytical p-values for ER models --

# Create a vector to store analytical p-values for each language.
p_values_analytical <- numeric(length(languages))
names(p_values_analytical) <- languages

# Loop over each language to compute the analytical p-values.
for (lang in languages) {
  
  # Get the real network's clustering coefficient for the current language.
  real_clustering <- clustering_results[clustering_results$Language == lang,]$RealNetworkClustering
  
  # Get the number of nodes and edges from the results dataframe for the current language.
  N = results[results$Language == lang,]$N
  E = results[results$Language == lang,]$E
  
  # Calculate the probability p for the ER graph.
  p = E / (N * (N - 1) / 2)
  
  # Calculate the expected clustering coefficient for the ER graph.
  C_ER = expected_clustering(p)
  
  # Calculate the variance of the clustering coefficient for the ER graph.
  variance_C_ER = variance_clustering(N, p)
  
  # Calculate the Z-score.
  Z = z_score(real_clustering, C_ER, variance_C_ER)
  
  # Compute the analytical p-value for the current language.
  p_values_analytical[lang] = p_value_analytical(Z)
}

# Print the computed analytical p-values.
print(p_values_analytical)




