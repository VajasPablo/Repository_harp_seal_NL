
# Function for add y axis column for key figure (x-axis are our ecological indicator)

Rank_avg_impact_function <- function(LoD_select) {
  
  nb_group_EwE_1985 = 49
  nb_group_EwE_2013 = 49
  nb_group_EwE_2020_2J3K = 42
  nb_group_EwE_2020_3LNO = 45
  
  column_name <-  c("EwE_Model", "Species", "Rank", "Avg_impact")
  
  # Naming the dataframes in the R environment, using the same name as the files without the .xlsx extension
  files <- list(
    "EwE_1985_capelin_raw_data_percent_change",
    "EwE_1985_cod_raw_data_percent_change",
    "EwE_1985_harp_seal_raw_data_percent_change",
    "EwE_2013_capelin_raw_data_percent_change",
    "EwE_2013_cod_raw_data_percent_change",
    "EwE_2013_harp_seal_raw_data_percent_change",
    "EwE_2020_2J3K_capelin_raw_data_percent_change",
    "EwE_2020_2J3K_cod_raw_data_percent_change",
    "EwE_2020_2J3K_harp_seal_raw_data_percent_change",
    "EwE_2020_3LNO_capelin_raw_data_percent_change",
    "EwE_2020_3LNO_cod_raw_data_percent_change",
    "EwE_2020_3LNO_harp_seal_raw_data_percent_change"
  )
  
  # Initialize a list to store the results
  results <- list()
  
  # Loop over each dataframe
  for (file in files) {
    # Check if the object exists in the environment
    if (exists(file)) {
      # Get the dataframe from the environment
      data <- get(file)
      
      # Create the dataframe with ranks
      df_rank <- data %>%
        select(!!LoD_select) %>% rename(LoD = !!LoD_select) %>%
        mutate(rank = case_when(
          abs(LoD) >= 0 & abs(LoD) < 20 ~ 0,
          abs(LoD) >= 20 & abs(LoD) < 40 ~ 1,
          abs(LoD) >= 40 & abs(LoD) < 60 ~ 2,
          abs(LoD) >= 60 ~ 3
        ))
      
      # Extract the unique name from the file
      unique_name <- str_remove(file, "_raw_data_percent_change$")
      
      # Separate the period and the species
      match <- str_match(unique_name, "^(EwE_\\d+(_[2-3][A-Z0-9]+)?)_(.*)$")
      period <- match[2]
      species <- match[4]
      
      # Create the result vector
      result <- c(
        "EwE_Model" = period,
        "Species" = species,
        "Rank" = max(df_rank$rank),
        "Avg_impact" = sum(df_rank$rank == 2)
      )
      
      # Add the vector to the results
      results[[length(results) + 1]] <- result
    }
  }
  
  # Convert the results list to a dataframe
  final_results <- do.call(rbind, results) %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  # Add column names
  colnames(final_results) <- column_name
  
  final_results <- final_results %>% mutate(
    EwE_Model = case_when(
      EwE_Model == "EwE_1985" ~ "EwE 1985",
      EwE_Model == "EwE_2013" ~ "EwE 2013",
      EwE_Model == "EwE_2020_2J3K" ~ "EwE 2020 2J3K",
      EwE_Model == "EwE_2020_3LNO" ~ "EwE 2020 3LNO"),
    Species = case_when(
      Species == "capelin" ~ "Capelin",
      Species == "cod" ~ "Cod> 35 cm",
      Species == "harp_seal" ~ "Harp seal",
    )) %>% 
    mutate(Avg_impact = as.numeric(Avg_impact)) %>%
    mutate(
      Avg_impact = case_when(
        EwE_Model == "EwE 1985" ~ Avg_impact / nb_group_EwE_1985,
        EwE_Model == "EwE 2013" ~ Avg_impact / nb_group_EwE_2013,
        EwE_Model == "EwE 2020 2J3K" ~ Avg_impact / nb_group_EwE_2020_2J3K,
        EwE_Model == "EwE 2020 3LNO" ~ Avg_impact / nb_group_EwE_2020_3LNO
      )
    )
  
  small_cod <- final_results %>%
    filter(Species == "Cod> 35 cm") %>%
    mutate(Species = "Cod<= 35 cm")
  
  final_results <- rbind.data.frame(final_results, small_cod)
  
  Species_scale_summary_table_large <- Species_scale_summary_table %>% 
    left_join(final_results, by = c("EwE_Model", "Species")) 
  
  assign("Species_scale_summary_table_large", Species_scale_summary_table_large, envir = .GlobalEnv)

}
