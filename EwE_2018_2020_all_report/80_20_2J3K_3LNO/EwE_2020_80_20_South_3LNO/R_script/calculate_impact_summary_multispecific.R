
### function in order to calculate all summaries for multipecific part ###

calculate_impact_summary_multispecific <- function(F_data, species_unselect, species_1, species_2, total_number_group, species_short) {
  
  df_group_impacted <- function(F_data, species_unselect){
    
    
    # Filter F_data and rename columns
    Df_percentage <- F_data %>% 
      filter(!(Group_number %in% species_unselect)) %>%
      select(-c(Group_number, Group_name)) %>% 
      rename_with(~ gsub("biomass_annual_", "", .), starts_with("biomass_annual_"))
    
    
    # Convert selected columns to numeric
    selected_columns_numeric <- apply(Df_percentage, 2, as.numeric)
    
    # Calculate percentage changes
    percentage_changes <- (selected_columns_numeric[, -1] - selected_columns_numeric[, 1]) / abs(selected_columns_numeric[, 1]) * 100
    
    # Create LoD_obs
    col_name <- paste0("LoD_", species_1, "_obs_LoD_", species_2, "_obs")
    new_column <- rep(0, nrow(Df_percentage))
    
    # add to result_df
    result_df <- cbind(F_data %>% filter(!(Group_number %in% species_unselect)) %>% select(Group_number, Group_name), 
                       setNames(data.frame(new_column), col_name), 
                       percentage_changes)
    
    # Start calculation
    
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
  result_df <- df_group_impacted(F_data, species_unselect)
  
  # Call summary_function with appropriate thresholds
  table_20 <- summary_function(Df_change(result_df, 20), total_number_group, 20)
  table_40 <- summary_function(Df_change(result_df, 40), total_number_group, 40)
  
  assign(paste0("Df_impacted_", species_short, "_table_20"), table_20, envir = .GlobalEnv)
  assign(paste0("Df_impacted_", species_short, "_table_40"), table_40, envir = .GlobalEnv)
  
  DT_tables <- list()
  DT_tables$T20 <- DT::datatable(data = table_20, filter="top",
                                 rownames = FALSE, options = list(pageLength = 6, scrollX=T),
                                 caption = paste0("For ", species_1, " verus ", species_2, " Table of the groups number and groups percentage impacted by more than 20% of total change"))
  
  DT_tables$T40 <- DT::datatable(data = table_40, filter="top",
                                 rownames = FALSE, options = list(pageLength = 6, scrollX=T),
                                 caption = paste0("For ", species_1, " verus ", species_2, " Table of the groups number and groups percentage impacted by more than 40% of total change"))
  
  object_name_DT <- paste0("DT_tables_", species_short)
  assign(object_name_DT, DT_tables, envir = .GlobalEnv)
  
  impacted_group_table <- result_df %>%
    mutate_at(vars(starts_with("LoD")), ~case_when(
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
