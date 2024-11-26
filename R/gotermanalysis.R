#---------------------------------------------------------#
#                   GO TERM ANALYSIS                      #
#---------------------------------------------------------#

# Load libraries
library(webchem)
# library(dplyr)
# library(rJava)
# library(rcdk)
# library(KEGGREST)

# Function to subset data based on some criteria in the identifier column
subset_data <- function(data, identifier_column) {
  # Example: Subset rows where the identifier is not NA
  subset <- data[!is.na(data[[identifier_column]]) &
                   data[[identifier_column]] != "" &
                   data[[identifier_column]] != " " &
                   data[[identifier_column]] != "N/A",]
  # Print the number of rows and columns
  cat("The number of rows and columns in the data are: ", nrow(subset),
      " and ", ncol(subset), "\n")
  return(subset)
}

# Main function to gather identifiers
gather_identifiers <- function(data, identifier_column) {
  # Initialize the results df
  identifier_df <- data.frame()
  # Use a progress bar to track progress
  shiny::withProgress(message = "Gathering identifiers...", value = 0, {
    for (i in 1:2) { # nrow(data)
      # Increment the progress bar
      incProgress(1 / nrow(data), detail = paste("Processing metabolite", i, "of", nrow(data)))
      
      # Get the start time
      start <- Sys.time()
      # Get the InChI from the data and other identifiers using webchem
      InChI <- data[[identifier_column]][i]
      # InChIKey <- tryCatch(cs_convert(InChI, from = "inchi", to = "inchikey"), error = function(e) NA)
      # SMILES <- tryCatch(cs_convert(InChI, from = "inchi", to = "smiles"), error = function(e) NA)
      CID <- tryCatch(get_cid(InChI, from = "inchi") %>% slice(1) %>% pull(cid), error = function(e) NA)
      # CSID <- tryCatch(get_csid(InChI, from = "inchi") %>% pull(csid), error = function(e) NA)
      IUPACName <- tryCatch(pc_prop(as.integer(CID)) %>% slice(1) %>% pull(IUPACName), error = function(e) NA)
      WDID <- tryCatch(pc_sect(CID, "wikidata")%>% slice(1) %>% pull(SourceID), error = function(e) NA)
      HMDBID <- tryCatch(pc_sect(CID, "hmdb id")%>% slice(1) %>% pull(Result), error = function(e) NA)
      CASID <- tryCatch(pc_sect(CID, "cas")%>% slice(1) %>% pull(Result), error = function(e) NA)
      KEGGID <- tryCatch(pc_sect(CID, "kegg id")%>% slice(1) %>% pull(Result), error = function(e) NA)
      ChEBIID <- tryCatch(get_chebiid(InChI, from = "inchi")%>% slice(1) %>% pull(chebiid), error = function(e) NA)
      
      # Append each row to identifier_df
      identifier_df <- rbind(identifier_df, data.frame(
        InChI = InChI,
        # InChIKey = InChIKey,
        # SMILES = SMILES,
        CID = CID,
        # CSID = CSID,
        IUPACName = IUPACName,
        WDID = WDID,
        HMDBID = HMDBID,
        CASID = CASID,
        KEGGID = KEGGID,
        ChEBIID = ChEBIID,
        stringsAsFactors = FALSE
      ))
      
      # Print runtime for debugging
      cat("Metabolite:", i, "Run Time:", Sys.time() - start, "seconds\n")
    }
  })
  
  # Return the identifier_df
  return(identifier_df)
}

# Function to merge data based on the identifier column
merge_data <- function(main_df, identifier_df, identifier_column) {
  
  # Ensure the identifier column has the same name in both data frames
  names(identifier_df)[1] <- identifier_column
  
  # Identify overlapping columns other than the identifier_column
  common_cols <- intersect(names(main_df), names(identifier_df))
  common_cols <- setdiff(common_cols, identifier_column)
  
  # Rename overlapping columns in identifier_df to avoid duplication
  if(length(common_cols) > 0) {
    names(identifier_df)[names(identifier_df) %in% common_cols] <- paste0(common_cols, "_id")
  }
  
  # Merge main_df and identifier_df on the identifier column
  merged_df <- merge(main_df, identifier_df, by = identifier_column, all.x = TRUE)
  
  # Get the position of the identifier_column in main_df
  identifier_index <- which(names(main_df) == identifier_column)
  
  # Get the names of the columns from identifier_df (excluding identifier_column)
  id_cols <- setdiff(names(identifier_df), identifier_column)
  
  # Build the new column order
  # Start with columns before the identifier_column in main_df
  if(identifier_index > 1) {
    left_cols <- names(main_df)[1:(identifier_index - 1)]
  } else {
    left_cols <- character(0)
  }
  
  # Columns after the identifier_column in main_df
  if(identifier_index < ncol(main_df)) {
    right_cols <- names(main_df)[(identifier_index + 1):ncol(main_df)]
  } else {
    right_cols <- character(0)
  }
  
  # New column order: left_cols, identifier_column, id_cols (from identifier_df), right_cols
  new_col_order <- c(left_cols, identifier_column, id_cols, right_cols)
  
  # Reorder the merged_df according to new_col_order
  final_df <- merged_df[, new_col_order]
  
  return(final_df)
}




