# #---------------------------------------------------------#
# #                Go Term Analysis Helper                  # 
# #---------------------------------------------------------#
# 
# # install.packages("httr")
# # install.packages("rvest")
# # install.packages("xml2")
# library(xml2)
# library(rvest)
# library(httr)
# library(stringr)
# 
# # SMILES TO CID ----
# query <- identifier_df$SMILES  # Example SMILES for ethanol
# url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/smiles/", query, "/cids/JSON")
# 
# response <- GET(url) # Send the request
# response # Check the response
# data <- content(response, "parsed") # Parse the response
# print(data) # Print the data
# data$IdentifierList$CID # Extract the CID
# 
# 
# # From InChI, InChIKey, SMILES, CID, CSID, IUPACName, ChEBIID to WDID, HMDB, CAS, KEGG, ----
# query <- identifier_df$chebiid 
# url <- paste0("https://www.ebi.ac.uk/chebi/searchId.do?chebiId=4167")
# page <- read_html(url)
# 
# xml_name(page)
# page_text <- xml_text(page)
# # Remove lines with common patterns in JavaScript, CSS, or HTML
# clean_text <- page_text %>%
#   str_replace_all("\\n", " ") %>%                    # Replace newlines with spaces for easier reading
#   str_replace_all("\\s{2,}", " ") %>%                # Collapse multiple spaces
#   str_remove_all("<[^>]+>") %>%                      # Remove any remaining HTML tags
#   str_remove_all("function .*?\\}") %>%              # Remove JavaScript functions
#   str_remove_all("document\\..*?;") %>%              # Remove JavaScript document-related commands
#   str_remove_all("\\$\\(.*?\\);")                # Remove jQuery-style commands
# print(clean_text)
# 
# kegg_name <- str_extract(clean_text, "C\\d{5}")
# kegg_name
# cas_number <- str_extract(clean_text, "\\d{2}-\\d{2}-\\d+")
# cas_number
# 
