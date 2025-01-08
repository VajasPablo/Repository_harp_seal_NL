
### function for import & transform ecopath output in monospecific part ###

import_transformation_output_function <- function(input_select, species_short, species_long, wich_simulation, F_vector, digit_import_value, print) {
  
  # input_select Prefix select good folder
  if (input_select == "biomass") {
    file_prefix <- "Biomass_output_"
  } else if (input_select == "catch") {
    file_prefix <- "Catch_output_"
  } else {
    stop("input_select must be 'biomass' or 'catch'")
  }
  
  # Found good digits number for import correctly xlsx files 
  
  if (wich_simulation == "final") {
    F_vector <- trimws(as.character(format(round(F_vector, digits = digit_import_value), nsmall = digit_import_value)))
  } else {
    F_vector
  }
  
  # Files name creation
  file_names <- paste0(input_select, "_annual_", species_short, "_", F_vector, ".xlsx")

    # Read xlsx with good prefix
  all_data <- lapply(file_names, function(file) {
    openxlsx::read.xlsx(here("EwE_output", species_long, paste0(file_prefix,  wich_simulation, "_simulation"), file))
  })
  
  # Initialize a list to store transformed dataframes
  list_of_dataframes <- list()
  
  # Iterate through each dataframe in all_data
  for (i in seq_along(all_data)) {
    # Create var_name for the current dataframe
    var_name <- as.list(paste0(input_select, "_annual_", species_short, "_", F_vector))
    
    # Retrieve the current dataframe
    simulation_data <- all_data[[i]]
    
    # Rename the 'var_name' column to the corresponding value in var_name list
    names(simulation_data)[names(simulation_data) == "var_name"] <- var_name[[i]]
    
    # Apply necessary transformations to the dataframe
    simulation_data <- simulation_data %>% 
      filter(row_number() %in% c(8, n())) %>% # filter(row_number() %in% c(13, n())) for version 6.7.0
      t() %>% 
      as_tibble() %>%
      dplyr::rename(Group_number = V1, !!var_name[[i]] := V2) %>%
      slice(-1) %>%
      as.data.frame() %>%
      mutate(Group_number = as.integer(Group_number))
    
    # Store the transformed dataframe in list_of_dataframes
    list_of_dataframes[[i]] <- simulation_data
  }
  
  # Merge the transformed dataframes
  join_column <- "Group_number"
  Df_merged <- reduce(list_of_dataframes, ~ dplyr::left_join(.x, .y, by = join_column), suffix = c(".x", ".y")) %>% left_join(group_name_key, by = "Group_number")
  
  # Assign the result to the global environment
  assign(paste0("Df_merged_", species_short, "_", input_select, "_", wich_simulation), Df_merged, envir = .GlobalEnv)
  
  if (print) {
    print(Df_merged)
  }
  
}
