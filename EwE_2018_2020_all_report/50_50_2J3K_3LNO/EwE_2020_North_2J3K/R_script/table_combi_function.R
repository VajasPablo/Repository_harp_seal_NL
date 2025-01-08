
### Function to generate table of each combination run in the ecosim simulation ### 

table_combi_function <- function(data, species_col, species_row) {
  
  data <- data %>% select(-c("Group_number", "Group_name"))
  col_names <- names(data)
  col_names <- gsub("^biomass_annual_", "", col_names)
  col_groups <- split(col_names, ceiling(seq_along(col_names)/6))  
  
  combined_col_names <- Reduce(function(x, y) cbind(x, y), col_groups) 
  
  colnames(combined_col_names) <- c(paste0(species_col, " LoD obs"), paste0(species_col, " LoD 0"), paste0(species_col, " LoD 25"), paste0(species_col, " LoD 50"), paste0(species_col, " LoD 75"), paste0(species_col, " LoD 99"))
  rownames(combined_col_names) <- c(paste0(species_row, " LoD obs"), paste0(species_row, " LoD 0"), paste0(species_row, " LoD 25"), paste0(species_row, " LoD 50"), paste0(species_row, " LoD 75"), paste0(species_row, " LoD 99"))
  
  datatable(data = combined_col_names, filter="top",
            rownames = TRUE, options = list(pageLength = 6, scrollX=T),
            caption = paste0("Each combination run in the ecosim simulation", species_row, " verus ", species_col))
  
}
