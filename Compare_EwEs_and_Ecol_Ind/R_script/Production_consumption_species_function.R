
# Production_consumption by species select 

Production_consumption_species_function <- function(EwE_period, data_raw, select_species) {
  
    # Import + clean
  if (EwE_period == "EwE_1985"){
    
    Df_clean <- openxlsx::read.xlsx(here("Ecol_ind_data_input", paste0(data_raw, ".xlsx")), colNames = TRUE, rowNames = FALSE)
    
  } else if (EwE_period == "EwE_2013") {
    
    Df_clean <- openxlsx::read.xlsx(here("Ecol_ind_data_input", paste0(data_raw, ".xlsx")), colNames = TRUE, rowNames = FALSE) %>%
      mutate(X2 = case_when(
        X2 == "Whales Minke" ~ "Whale Minke",
        X2 == "Seabird benthic invert feeder" ~ "Seabird benthivore",
        X2 == "Cod > 35" ~ "Cod> 35 cm",
        X2 == "Cod<=35" ~ "Cod<= 35 cm",
        X2 == "Greenland Halibut " ~ "Greenland halibut",
        X2 == "Silver hake/saithe" ~ "Silver hake/ Saithe",
        X2 == "Other Piscivorous fish" ~ "Other piscivorous fish",
        X2 == "American Plaice > 35" ~ "AmPlaice > 35",
        X2 == "American Plaice <= 35" ~ "AmPLaice <= 35",
        X2 == "Micro zooplankton" ~ "Microzooplankton",
        X2 == "Phytoplankton L" ~ "Large Phytoplankton",
        X2 == "Phytoplankton S" ~ "Small Phytoplankton",
        TRUE ~ X2))
  } else {
    
    Df_clean <- openxlsx::read.xlsx(here("Ecol_ind_data_input", paste0(data_raw, ".xlsx")), colNames = TRUE, rowNames = FALSE) %>%
      mutate(X2 = case_when(
        X2 == "Atlantic cod greater than 35cm" ~ "Cod> 35 cm",
        X2 == "Atlantic cod less than 35cm" ~ "Cod<= 35 cm",
        X2 == "Seal harp" ~ "Seal Harp",
        X2 == "Whale zooplankton eater" ~ "Whale zp eater",
        X2 == "Seal hooded" ~ "Seal Hooded",
        X2 == "Silver hake and pollock" ~ "Silver hake/ Saithe",
        X2 == "Other plank-piscivorous fish" ~ "Other plank-pisc fish",
        X2 == "American plaice greater than 35cm" ~ "AmPlaice > 35",
        X2 == "American plaice less than 35cm" ~ "AmPLaice <= 35",
        X2 == "Other large benthivorous fish" ~ "Other L benthivorous fish",
        X2 == "Other medium benthivorous fish" ~ "Other M benthivorous fish",
        X2 == "Small benthivorous fish" ~ "Other S benthivorous fish",
        X2 == "Large phytoplankton" ~ "Large Phytoplankton",
        X2 == "Small phytoplankton" ~ "Small Phytoplankton",
        TRUE ~ X2))
  }
  
  Species_names <- as.character(Df_clean[1,-1])
  Df_clean <- Df_clean[-1, -1]
  Df_clean <- Df_clean %>%
    rename_with(~ Species_names, everything())
  
  # select trophic level column
  
  PQ_sp_index <- Df_clean %>% select("Group name","Production / consumption (/year)") %>% 
    filter(`Group name` %in% select_species) %>% 
    select("Production / consumption (/year)") %>% 
    pull() %>% as.numeric()
  
  # save in globalenv
  assign(paste0(EwE_period, "_Production_consumption_", select_species), PQ_sp_index, envir = .GlobalEnv)
  
  for_print <- c(select_species,PQ_sp_index)
  
  print(for_print)
  
}
