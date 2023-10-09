#----- create table 
setwd("~/GitHub/LAB-3-5-Oct-Significance-of-network-metrics/")


library(xtable)

# Define the list of languages
languages <- c("Arabic", "Basque", "Catalan", "Chinese", "Czech", 
               "English", "Greek", "Hungarian", "Italian", "Turkish")

# Create an empty list to store the data frames
data_list <- list()

# Create an empty dataframe to store information about different degree sequences
results <- data.frame(`Language` = character(0), 
                      `N` = numeric(0),
                      `E` = numeric(0),
                      `<k>` = numeric(0),
                      `δ` = numeric(0),
                      stringsAsFactors = FALSE)

# N -> number of vertices
# E -> number of edges
# <k> (=2E/N) -> mean degree
# δ (=2E/(N(N-1))) -> network density

for (lang in languages) {
  file_name <- paste0("data/", lang, "_syntactic_dependency_network.txt")
  
  # Read the file
  lines <- readLines(file_name)
  
  # Extract number of vertices and edges from the first line
  vertices_edges <- as.numeric(unlist(strsplit(lines[1], " ")))
  num_vertices <- vertices_edges[1]
  num_edges <- vertices_edges[2]
  
  # Process the remaining lines to extract pairs of linked vertices
  network_data <- do.call(rbind, lapply(lines[-1], function(line) {
    unlist(strsplit(line, " ", fixed = TRUE))
  }))
  
  # Filter out loops
  network_data <- network_data[network_data[,1] != network_data[,2],]
  
  # Convert to a data frame
  network_data <- as.data.frame(network_data, stringsAsFactors = FALSE)
  
  # Save the data frame to the global environment with a name based on the language
  assign(paste0(tolower(lang), "_data"), network_data, envir = .GlobalEnv)
  
  # Also store it in the data_list
  data_list[[tolower(lang)]] <- network_data
  
  # Calculate <k> and δ
  mean_degree <- 2 * num_edges / num_vertices
  network_density <- 2 * num_edges / (num_vertices * (num_vertices - 1))
  
  # Append to results dataframe
  results <- rbind(results, data.frame(`Language` = lang, 
                                       `N` = num_vertices,
                                       `E` = num_edges,
                                       `<k>` = mean_degree,
                                       `δ` = network_density))
}


# Export the table for LaTeX
# Rename the columns for LaTeX output
colnames(results) <- c("Language", "N", "E", "<k>", "δ")

# Export the table for LaTeX
# Convert the results dataframe to a LaTeX table
latex_table <- xtable::xtable(results)

# Print the LaTeX table to a file
output_file <- "results_table_1.tex"
print(latex_table, type = "latex", file = output_file, floating = FALSE)




