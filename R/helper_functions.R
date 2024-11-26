# # Helper functions 
# # setwd("~/Desktop/SDU/Cand/Master thesis /Datasets/Liverfetus")
# # data <- read.csv("Liverfetus_positive.csv", header = TRUE, sep = ",")
# # sequence <- read.csv("Liverfetus_positive_seq.csv", header = TRUE, sep = ",")
# 
# modify_sample_name <- function(sequence) {
#   # Check if the 'labels' column in sequence contains the value 'sample'
#   if ("labels" %in% colnames(sequence)) {
#     print("Labels column found in sequence.")
#     for (i in 1:nrow(sequence)) {
#       if (sequence[i, 'labels'] == 'sample') {
#         sequence[i, 'sample'] <- paste0("X", sequence[i, 'sample'])
#       }
#     }
#   } else {
#     print("Labels column not found in sequence.")
#   }
#   return(sequence)
# }
# 
# filter_group <- function(data, sequence) {
#   print("Filtering data...")
#   
#   # Check if 'labels' column exists and has "sample" values
#   if (!"labels" %in% colnames(sequence)) {
#     print("Error: 'labels' column not found in sequence data.")
#     return(list(data = NULL, sequence = NULL))
#   }
#   
#   # Print unique values in 'labels' to verify the filtering condition
#   print("Unique values in sequence$labels:")
#   print(unique(sequence$labels))
#   
#   # Filter based on "sample" label
#   filtered_data <- data[, sequence$labels %in% c("sample"), drop = FALSE]
#   filtered_sequence <- sequence[sequence$labels %in% c("sample"), , drop = FALSE]
#   
#   print("Filtered data dimensions:")
#   print(dim(filtered_data))
#   print("Filtered sequence dimensions:")
#   print(dim(filtered_sequence))
#   
#   # If filtered data is empty, log and return empty result
#   if (ncol(filtered_data) == 0 || nrow(filtered_sequence) == 0) {
#     print("Warning: No matching 'sample' labels found in data or sequence.")
#     return(list(data = NULL, sequence = NULL))
#   }
#   
#   # Debugging before transformation
#   print("Number of rows before cleaning:")
#   print(nrow(filtered_data))
#   
#   # Set unique row names and perform additional data transformations
#   if ("Name" %in% colnames(data)) {
#     unique_row_names <- make.unique(as.character(data[, "Name"]))
#     rownames(filtered_data) <- unique_row_names
#     filtered_data <- filtered_data[, -1, drop = FALSE]
#   } else {
#     print("Warning: 'Name' column not found in data.")
#   }
#   
#   # Log-transform data and clean
#   # filtered_data <- log2(filtered_data)
#   filtered_data <- na.omit(filtered_data)
#   
#   # Debugging after cleaning
#   print(paste("Number of rows after cleaning:", nrow(filtered_data)))
#   print(paste("Filtered data final dimensions:", dim(filtered_data)))
#   print(paste("Filtered sequence final dimensions:", dim(filtered_sequence)))
#   
#   return(list(data = filtered_data, sequence = filtered_sequence))
# }
# 
# # Usage example
# # filtered_result <- filter_group(data, sequence)
# # filtered_data <- filtered_result$data
# # filtered_sequence <- filtered_result$sequence
