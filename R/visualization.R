# # ------------- # 
# # Visualization #
# # ------------- # 
# library(heatmaply)
# 
# # Heatmap ---------------------------
# # Gradient color for heatmap
# gradient_col <- ggplot2::scale_fill_gradient2(
#   low = "blue", high = "red", 
#   midpoint = 0.5, limits = c(-3, 3)
# )
# 
# # Function to calculate feature variability and select top features
# select_top_features <- function(data, top_n = 20) {
#   feature_variability <- apply(data, 1, sd)  # Calculate variability across samples (rows)
#   sorted_features <- names(sort(feature_variability, decreasing = TRUE))
#   
#   # Handle cases where top_n exceeds the number of available features
#   if (top_n > length(sorted_features)) {
#     top_n <- length(sorted_features)
#   }
#   
#   most_variable_features <- sorted_features[1:top_n]
#   print(most_variable_features)  # Debug print to check selected features
#   most_variable_features
# }
# 
# # Function to create heatmaply plot
# create_heatmap <- function(data, gradient_col, title, dendrogram_option) {
#   heatmaply(
#     data,
#     k_col = 1,  # Number of columns to display
#     k_row = 1,  # Number of clusters for rows
#     scale_fill_gradient_fun = gradient_col,  # Apply defined color gradient
#     dendrogram = dendrogram_option,  # Display selected dendrogram option
#     margins = c(NA, NA, NA, NA),  # Margin settings
#     main = title,  # Main title
#     scale = "none"  # No scaling applied
#   )
# }
# 
# # Volcano --------------------------- 
# perform_statistical_testing <- function(data, sequence_df, group1, group2) {
#   # Filter data and sequence based on groups
#   condition_runs1 <- sequence_df$sample[sequence_df$class == group1]
#   condition_runs2 <- sequence_df$sample[sequence_df$class == group2]
#   
#   # Debug: Print selected condition runs
#   print("Condition runs for group1:")
#   print(condition_runs1)
#   print("Condition runs for group2:")
#   print(condition_runs2)
#   
#   # Ensure the selected columns are present in the data
#   condition_runs1 <- intersect(condition_runs1, colnames(data))
#   condition_runs2 <- intersect(condition_runs2, colnames(data))
#   
#   # Debug: Print intersected condition runs
#   print("Intersected condition runs for group1:")
#   print(condition_runs1)
#   print("Intersected condition runs for group2:")
#   print(condition_runs2)
#   
#   # Subset the data based on the intersected condition runs
#   condition_data1 <- data[, condition_runs1, drop = FALSE]
#   condition_data2 <- data[, condition_runs2, drop = FALSE]
#   
#   # Create dataframes for each condition
#   df_1 <- condition_data1
#   df_2 <- condition_data2
#   
#   # Debug: Print column names and dimensions
#   print("Columns in condition_data1:")
#   print(colnames(condition_data1))
#   print("Columns in condition_data2:")
#   print(colnames(condition_data2))
#   print("Dimensions of condition_data1:")
#   print(dim(condition_data1))
#   print("Dimensions of condition_data2:")
#   print(dim(condition_data2))
#   
#   # Prepare ANOVA data
#   ANOVA_data <- data
#   ANOVA_data$Metabolite <- rownames(ANOVA_data)
#   ANOVA_data_long <- melt(ANOVA_data, id.vars = "Metabolite", variable.name = "sample", value.name = "Expression")
#   
#   # Debug: Print dimensions of melted ANOVA_data
#   print("Dimensions of ANOVA_data_long after melt:")
#   print(dim(ANOVA_data_long))
#   
#   # Ensure that the sequence_df contains unique sample values
#   sequence_df <- sequence_df[!duplicated(sequence_df$sample), ]
#   
#   # Merge to add class information
#   ANOVA_data_long <- merge(ANOVA_data_long, sequence_df[, c("sample", "class")], by = "sample", all.x = TRUE)
#   
#   # Debug: Print dimensions after merge
#   print("Dimensions of ANOVA_data_long after merge:")
#   print(dim(ANOVA_data_long))
#   
#   # Ensure 'class' is treated as a factor
#   ANOVA_data_long$class <- factor(ANOVA_data_long$class)
#   
#   # Debug: Print unique classes
#   print("Unique classes in ANOVA_data_long$class:")
#   print(unique(ANOVA_data_long$class))
#   
#   # Levene's test for homogeneity of variances
#   levene_results <- data.frame(Metabolite = character(), p_value = numeric(), stringsAsFactors = FALSE)
#   for (metabolite in unique(ANOVA_data_long$Metabolite)) {
#     metabolite_data <- subset(ANOVA_data_long, Metabolite == metabolite)
#     levene_result <- leveneTest(Expression ~ class, data = metabolite_data)
#     p_value_levene <- levene_result$Pr[1]
#     levene_results <- rbind(levene_results, data.frame(Metabolite = metabolite, p_value = p_value_levene))
#   }
#   
#   # ANOVA test
#   results_df_anova <- data.frame(Metabolite = character(), p_value = numeric(), stringsAsFactors = FALSE)
#   for (metabolite in unique(ANOVA_data_long$Metabolite)) {
#     metabolite_data <- subset(ANOVA_data_long, Metabolite == metabolite)
#     model <- oneway.test(Expression ~ class, data = metabolite_data, var.equal = TRUE)
#     p_value <- model$p.value
#     results_df_anova <- rbind(results_df_anova, data.frame(Metabolite = metabolite, p_value = p_value))
#   }
#   
#   # Tukey's test
#   results_df_tukey <- data.frame(Metabolite = character(), Comparison = character(), diff = numeric(), lwr = numeric(), upr = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
#   for (metabolite in unique(ANOVA_data_long$Metabolite)) {
#     metabolite_data <- subset(ANOVA_data_long, Metabolite == metabolite)
#     is_significant <- results_df_anova$p_value[results_df_anova$Metabolite == metabolite] < 0.05
#     if (is_significant) {
#       tukey_result <- TukeyHSD(aov(Expression ~ class, data = metabolite_data))
#       comparisons <- as.data.frame(tukey_result$class[, c("diff", "lwr", "upr", "p adj")])
#       comparisons$Metabolite <- metabolite
#       comparisons$Comparison <- rownames(comparisons)
#       results_df_tukey <- rbind(results_df_tukey, comparisons)
#     }
#   }
#   
#   # Split dataframes by Comparison
#   list_of_comparison_dataframes <- split(results_df_tukey, results_df_tukey$Comparison)
#   
#   # Filter non-significant proteins
#   non_significant_proteins <- results_df_anova$Metabolite[results_df_anova$p_value >= 0.05]
#   for (i in seq_along(list_of_comparison_dataframes)) {
#     list_of_comparison_dataframes[[i]] <- list_of_comparison_dataframes[[i]][!list_of_comparison_dataframes[[i]]$Metabolite %in% non_significant_proteins, ]
#   }
#   
#   # Calculate Log2 Fold change for each comparison
#   for (comparison in names(list_of_comparison_dataframes)) {
#     comparison_df <- list_of_comparison_dataframes[[comparison]]
#     # Ensure the dimensions match before adding Log2FC column
#     if (nrow(comparison_df) > 0) {
#       comparison_df$Log2FC <- log2(rowMeans(df_2[comparison_df$Metabolite, , drop = FALSE]) / rowMeans(df_1[comparison_df$Metabolite, , drop = FALSE]))
#     } else {
#       print(paste("Skipping Log2FC calculation for comparison", comparison, "due to dimension mismatch"))
#     }
#     rownames(comparison_df) <- comparison_df$Metabolite
#     list_of_comparison_dataframes[[comparison]] <- comparison_df
#   }
#   
#   return(list(tukey_results = results_df_tukey, comparison_dfs = list_of_comparison_dataframes))
# }
# 
# create_volcano_plot <- function(results_df, group1, group2, log2fc_threshold, pval_threshold) {
#   # Create a new column for coloring points
#   results_df$color <- "Below Threshold"
#   results_df$color[results_df$`p adj` < pval_threshold & results_df$Log2FC > log2fc_threshold] <- "Upregulated"
#   results_df$color[results_df$`p adj` < pval_threshold & results_df$Log2FC < -log2fc_threshold] <- "Downregulated"
#   
#   # Create the volcano plot
#   volcano_plot <- ggplot(results_df, aes(x = Log2FC, y = -log10(`p adj`))) +
#     geom_point(aes(color = color), size = 1) +
#     xlim(c(min(results_df$Log2FC) - 0.2, max(results_df$Log2FC) + 0.2)) +
#     ylim(c(0, max(-log10(results_df$`p adj`)) + 1)) +
#     geom_hline(yintercept = pval_threshold, linetype = "dashed", color = "black") +
#     geom_vline(xintercept = c(-log2fc_threshold, log2fc_threshold), linetype = "dashed", color = "black") + # Add vertical lines for log2FC thresholds
#     labs(title = paste("Volcano Plot", group2, "vs", group1), x = "Log2 Fold Change", y = "-log10(p-value)") +
#     theme_bw() +
#     scale_color_manual(values = c("Upregulated" = "red", "Downregulated" = "blue", "Below Threshold" = "grey")) +
#     guides(color = guide_legend(title = NULL)) +
#     geom_text(data = subset(results_df, color == "Upregulated"), aes(label = Metabolite), color = "darkred", size = 2, nudge_x = 0.1, nudge_y = 0.2) + # Label upregulated points
#     geom_text(data = subset(results_df, color == "Downregulated"), aes(label = Metabolite), color = "darkblue", size = 2, nudge_x = -0.1, nudge_y = 0.2) # Label downregulated points
#   
#   ggplotly(volcano_plot)
# }