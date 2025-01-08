
### function to calculate and generate all summary % groups impacted and final table ###

calculate_impact_summary_LoR <- function(LoR_rename, b_data, species_unselect, LoR_obs_position, species, total_number_group, species_short) {
  
  LoR_obs_position_2 <- LoR_obs_position
  
  df_group_impacted <- function(LoR_rename, b_data, species_unselect){
    
    # Extract column names
    column_names <- paste0("LoR_", LoR_rename)
    
    # Filter b_data and rename columns
    Df_percentage <- b_data %>% 
      filter(!(Group_number %in% species_unselect)) %>%
      select(-c(Group_number, Group_name)) %>% 
      rename_at(vars(starts_with("b")), list(~ column_names)) %>% select(LoR_obs, LoR_1, LoR_10, LoR_20, LoR_30, 
                                                                         LoR_40, LoR_50, LoR_60, LoR_70, LoR_80, 
                                                                         LoR_90, LoR_100)
    # Convert selected columns to numeric
    selected_columns_numeric <- apply(Df_percentage, 2, as.numeric)
    
    # Calculate percentage changes
    percentage_changes <- (selected_columns_numeric[, -1] - selected_columns_numeric[, 1]) / abs(selected_columns_numeric[, 1]) * 100
    
    # Create column for LoR_0
    LoR_obs <- rep(0, nrow(Df_percentage))
    
    # Combine dataframes
    result_df <- cbind(b_data %>% filter(!(Group_number %in% species_unselect)) %>% select(Group_number, Group_name), 
                       LoR_obs, 
                       percentage_changes) %>% dplyr::relocate(LoR_obs, .before = LoR_obs_position_2)

  }
  
  Df_change <- function(df, threshold) {
    df %>%
      select(-c(Group_number, Group_name)) %>%
      mutate_if(is.numeric, ~ case_when(
        . >= threshold ~ "+",
        . <= -threshold ~ "-",
        TRUE ~ "no"
      ))
  }
  
  summary_function <- function(df_change, total_number_group, threshold) {
    df_change %>%
      mutate_all(~ as.numeric(. == "+" | . == "-")) %>%
      summarise(across(everything(), ~ ifelse(all(is.na(.)), NA, round(sum(., na.rm = TRUE), 2)))) %>%
      bind_rows(
        data.frame(t(round(colSums(df_change == "+"), 2)), Description = "Count of +"),
        data.frame(t(round(colSums(df_change == "-"), 2)), Description = "Count of -")
      ) %>%
      mutate(Description = coalesce(Description, "Total Count ±")) %>%
      bind_rows(
        df_change %>%
          mutate_all(~ as.numeric(. == "+" | . == "-")) %>%
          summarise(across(everything(), ~ ifelse(all(is.na(.)), NA, round(sum(., na.rm = TRUE) / total_number_group, 2)))) %>%
          bind_rows(
            data.frame(t(round(colSums(df_change == "+") / total_number_group, 2)), Description = "Prop. of +"),
            data.frame(t(round(colSums(df_change == "-") / total_number_group, 2)), Description = "Prop. of -")
          ) %>%
          mutate(Description = coalesce(Description, "Total Prop. ±"))
      ) %>%
      dplyr::relocate(Description, .before = 1) # move Description columne at first position 
  }
  
  # Call df_group_impacted function
  result_df <- df_group_impacted(LoR_rename, b_data, species_unselect)
  
  # Call summary_function with appropriate thresholds
  table_20 <- summary_function(Df_change(result_df, 20), total_number_group, 20)
  table_40 <- summary_function(Df_change(result_df, 40), total_number_group, 40)
  
  assign(paste0("Df_impacted_", species_short, "_table_20"), table_20, envir = .GlobalEnv)
  assign(paste0("Df_impacted_", species_short, "_table_40"), table_40, envir = .GlobalEnv)
  
  DT_tables <- list()
  DT_tables$T20 <- DT::datatable(data = table_20, filter="top",
                                 rownames = FALSE, options = list(pageLength = 6, scrollX=T),
                                 caption = paste0("For ", species, " Table of the groups number and groups percentage impacted by more than 20% of total change"))
  
  DT_tables$T40 <- DT::datatable(data = table_40, filter="top",
                                 rownames = FALSE, options = list(pageLength = 5, scrollX=T),
                                 caption = paste0("For ", species, " Table of the groups number and groups percentage impacted by more than 40% of total change"))
  
  object_name_DT <- paste0("DT_tables_", species_short)
  assign(object_name_DT, DT_tables, envir = .GlobalEnv)
  
  # Start calculation large table 
  
  impacted_group_table <- result_df %>%
    mutate_at(vars(starts_with("LoR")), ~case_when(
      . == 0 ~ "0",
      . > 0 & . <= 20 ~ "+",
      . > 20 & . <= 40 ~ "+ +",
      . > 40 ~ "+ + +",
      . >= -20 & . < 0 ~ "-",
      . < -20 & . >= -40 ~ "- -",
      . < -40 ~ "- - -",
      TRUE ~ as.character(.)
    )) %>%
    select(-Group_number) 
  
  object_name <- paste0("Df_impacted_", species_short, "_large_table")
  assign(object_name, impacted_group_table, envir = .GlobalEnv)
  write.xlsx(get(object_name), file = here("R_results_export", paste0(object_name, ".xlsx")), rowNames = FALSE)
  
}