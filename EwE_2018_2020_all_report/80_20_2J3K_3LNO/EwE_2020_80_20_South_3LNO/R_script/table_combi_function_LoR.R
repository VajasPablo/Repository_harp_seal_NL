
### Function to generate table of each combination run in the ecosim simulation ### 

table_combi_function_LoR <- function(data, species_col, species_row) {
  
  data <- data %>% select(-c("Group_number", "Group_name"))
  col_names <- names(data)
  col_names <- gsub("^biomass_annual_", "", col_names)
  col_groups <- split(col_names, ceiling(seq_along(col_names)/6))  
  
  combined_col_names <- Reduce(function(x, y) cbind(x, y), col_groups) 
  
  colnames(combined_col_names) <- c(paste0(species_col, " LoR obs"), paste0(species_col, " LoR 1"), paste0(species_col, " LoR 25"), paste0(species_col, " LoR 50"), paste0(species_col, " LoR 75"), paste0(species_col, " LoR 100"))
  rownames(combined_col_names) <- c(paste0(species_row, " LoR obs"), paste0(species_row, " LoR 1"), paste0(species_row, " LoR 25"), paste0(species_row, " LoR 50"), paste0(species_row, " LoR 75"), paste0(species_row, " LoR 100"))
  
  datatable(data = combined_col_names, filter="top",
            rownames = TRUE, options = list(pageLength = 6, scrollX=T),
            caption = paste0("Each combination run in the ecosim simulation", species_row, " verus ", species_col))
  
}
