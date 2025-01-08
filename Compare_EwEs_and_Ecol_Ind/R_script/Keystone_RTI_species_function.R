
# Omnivory Index by species select 

Keystone_RTI_species_function <- function(EwE_period, data_raw, select_species) {
  
  # Import + clean
  if (EwE_period == "EwE_1985"){
    
    Df_clean <- openxlsx::read.xlsx(here("Ecol_ind_data_input", paste0(data_raw, ".xlsx")), colNames = TRUE, rowNames = FALSE)
    
  } else if (EwE_period == "EwE_2013") {
    
    Df_clean <- openxlsx::read.xlsx(here("Ecol_ind_data_input", paste0(data_raw, ".xlsx")), colNames = TRUE, rowNames = FALSE) %>%
      mutate(`Group.name` = case_when(
        `Group.name` == "Whales Minke" ~ "Whale Minke",
        `Group.name` == "Seabird benthic invert feeder" ~ "Seabird benthivore",
        `Group.name` == "Cod > 35" ~ "Cod> 35 cm",
        `Group.name` == "Cod<=35" ~ "Cod<= 35 cm",
        `Group.name` == "Greenland Halibut " ~ "Greenland halibut",
        `Group.name` == "Silver hake/saithe" ~ "Silver hake/ Saithe",
        `Group.name` == "Other Piscivorous fish" ~ "Other piscivorous fish",
        `Group.name` == "American Plaice > 35" ~ "AmPlaice > 35",
        `Group.name` == "American Plaice <= 35" ~ "AmPLaice <= 35",
        `Group.name` == "Micro zooplankton" ~ "Microzooplankton",
        `Group.name` == "Phytoplankton L" ~ "Large Phytoplankton",
        `Group.name` == "Phytoplankton S" ~ "Small Phytoplankton",
        TRUE ~ `Group.name`))
    
  }  else {
    
    Df_clean <- openxlsx::read.xlsx(here("Ecol_ind_data_input", paste0(data_raw, ".xlsx")), colNames = TRUE, rowNames = FALSE) %>%
      mutate(`Group.name` = case_when(
        `Group.name` == "Atlantic cod greater than 35cm" ~ "Cod> 35 cm",
        `Group.name` == "Atlantic cod less than 35cm" ~ "Cod<= 35 cm",
        `Group.name` == "Seal harp" ~ "Seal Harp",
        `Group.name` == "Whale zooplankton eater" ~ "Whale zp eater",
        `Group.name` == "Seal hooded" ~ "Seal Hooded",
        `Group.name` == "Silver hake and pollock" ~ "Silver hake/ Saithe",
        `Group.name` == "Other plank-piscivorous fish" ~ "Other plank-pisc fish",
        `Group.name` == "American plaice greater than 35cm" ~ "AmPlaice > 35",
        `Group.name` == "American plaice less than 35cm" ~ "AmPLaice <= 35",
        `Group.name` == "Other large benthivorous fish" ~ "Other L benthivorous fish",
        `Group.name` == "Other medium benthivorous fish" ~ "Other M benthivorous fish",
        `Group.name` == "Small benthivorous fish" ~ "Other S benthivorous fish",
        `Group.name` == "Large phytoplankton" ~ "Large Phytoplankton",
        `Group.name` == "Small phytoplankton" ~ "Small Phytoplankton",
        TRUE ~ `Group.name`))
  }
  
  
  Species_names <- c("Group_name", "Keystone_index_1", "Keystone_index_2", "Relative_total_impact", "Keystone_index_3")
  
  Df_clean <- Df_clean %>%
    rename_with(~ Species_names, everything())
  
  # select trophic level column
  
  Key_RIT_sp_index <- Df_clean %>% select("Group_name","Keystone_index_1", "Keystone_index_2", "Relative_total_impact") %>% 
    filter(Group_name %in% select_species) 
  
  # save in globalenv
  assign(paste0(EwE_period, "_Keystone_RTI_", select_species), Key_RIT_sp_index, envir = .GlobalEnv)
  
  for_print <- c(Key_RIT_sp_index)
  
  print(for_print)  
}

