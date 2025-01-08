
### function to deal wwith multispe input generate ###

combined_simulation_generate_function <- function(simulation_names, Weight, Group_no, Type, time_step_start, nb_time_steps, fill_value_min, fill_value_max, output_file, species_1, species_1_number, F_species_1, species_2, species_2_number, F_species_2, order_colonnes) {
  # Create fixed columns
  fix_col <- c("Group", "Weight", "Group no.", "Type", time_step_start:max(nb_time_steps))
  
  # Use mapply to apply the generate_simulation_column function to each set of parameters
  sim_cols <- mapply(function(sim_name, weight, group_no, type, nb_time_step, fill_value_min, fill_value_max) {
    # Generate a column for a single simulation
    Sim_col <- c(sim_name, as.character(weight), as.character(group_no), as.character(type), 
                 as.character(seq(fill_value_min, fill_value_max, length.out = nb_time_step/2)),
                 rep(as.character(fill_value_max), nb_time_step/2))
    return(Sim_col)
  }, simulation_names, Weight, Group_no, Type, nb_time_steps, fill_value_min, fill_value_max, SIMPLIFY = FALSE)
  
  # Combine fixed columns with generated columns
  simulation_data <- cbind.data.frame(fix_col, do.call(cbind, sim_cols))
  colnames(simulation_data) <- NULL
  
  
  add_obs_column <- function(simulation_data, species_1, species_1_number, F_species_1, species_2, species_2_number, F_species_2, order_colonnes){
    
    species_1_init <- as.character(c(paste0("F",species_1, "_obs"), 1, species_1_number, 4))
    species_2_init <- as.character(c(paste0("F",species_2, "_obs"), 1, species_2_number, 4))
    
    species_1_initialisation <- c(species_1_init, rep(F_species_1, 100)) %>% 
      as.data.frame() %>% rename(species_1_initialisation = ".")
    colnames(species_1_initialisation) <- NULL
    
    species_2_initialisation <- c(species_2_init, rep(F_species_2, 100)) %>% 
      as.data.frame() %>% rename(species_2_initialisation = ".")
    colnames(species_2_initialisation) <- NULL
    
    
    simulation_data <- cbind(simulation_data,
                             species_1_initialisation,
                             species_2_initialisation)
    colnames(simulation_data) <- NULL
    
    simulation_data <- simulation_data[, order_colonnes]
    
  }
  
  simulation_data <- add_obs_column(simulation_data, species_1, species_1_number, F_species_1, species_2, species_2_number, F_species_2, order_colonnes)
  
  write.csv(simulation_data, file = output_file, row.names = FALSE)
  
  return(simulation_data)
  
}