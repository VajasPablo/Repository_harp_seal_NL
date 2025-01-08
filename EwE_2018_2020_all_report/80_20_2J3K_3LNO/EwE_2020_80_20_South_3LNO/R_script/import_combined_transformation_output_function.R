
### function to import and deal with multispecific simulation ###

import_combined_transformation_output_function <- function(input_select, species_short, species_short_2, folder, wich_simulation, F_vector, F_vector_2, obs_F_vector, obs_F_vector_2, digit_import_value, digit_import_value_2, LoD_vector_name, change_name_for_LoD, print) {
  
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
    F_vector <- as.character(format(round(F_vector, digits = digit_import_value), nsmall = digit_import_value))
  } else {
    F_vector
  }
  
  if (wich_simulation == "final") {
    F_vector_2 <- as.character(format(round(F_vector_2, digits = digit_import_value_2), nsmall = digit_import_value_2))
  } else {
    F_vector_2
  }
  
  obs_F_vector <-  as.character(format(obs_F_vector, digits = digit_import_value, nsmall = digit_import_value)) %>% 
    unlist() %>% 
    unname()
  obs_F_vector_2 <-  as.character(format(obs_F_vector_2, digits = digit_import_value_2, nsmall = digit_import_value_2)) %>% 
    unlist() %>% 
    unname()
  
  F_vector <- c(obs_F_vector, F_vector)
  F_vector_2 <- c(obs_F_vector_2, F_vector_2)
  
  # Files name creation
  #####################
  
  file_names1 <- paste0(input_select, "_annual_", "F", species_short, "_", F_vector)
  file_names2 <- paste0("F", species_short_2, "_", F_vector_2)
  file_names_expand <- expand.grid(file_names1 = file_names1, file_names2 = file_names2)
  file_names_final <- file_names_expand %>% 
    mutate(final_name = paste0(file_names1, "_" ,file_names2, ".xlsx")) %>% 
    select(final_name) %>% unname() %>% unlist()

  # Read xlsx with good prefix
  all_data <- lapply(file_names_final, function(file) {
    openxlsx::read.xlsx(here("EwE_output", folder, paste0(file_prefix,  wich_simulation, "_simulation"), file))
  })
  
  # Initialize a list to store transformed dataframes
  list_of_dataframes <- list()
  
  # Iterate through each dataframe in all_data
  for (i in seq_along(all_data)) {
    # Create var_name for the current dataframe 
    var_name <- file_names_expand %>% 
      mutate(final_name = paste0(file_names1, "_" ,file_names2)) %>% 
      select(final_name) %>% unname() %>% unlist() %>% as.list()
    
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
  
  
  file_names1 <- paste0(input_select, "_annual_", "LoD_", species_short, "_", LoD_vector_name)
  file_names2 <- paste0("LoD_", species_short_2, "_", LoD_vector_name)
  file_names_expand <- expand.grid(file_names1 = file_names1, file_names2 = file_names2)
  file_names_final <- file_names_expand %>% 
    mutate(final_name = paste0(file_names1, "_" ,file_names2)) %>% 
    select(final_name) %>% unname() %>% unlist()
  new_colname <- c("Group_number" ,file_names_final, "Group_name")
  
  
  if (change_name_for_LoD) {
    Df_merged <- setNames(Df_merged, new_colname)
  }
  
  
  # Assign the result to the global environment
  assign(paste0("Df_merged_", species_short, "_vs_", species_short_2, "_", input_select, "_", wich_simulation), Df_merged, envir = .GlobalEnv)
  
  if (print) {
    print(Df_merged)
  }
  
}