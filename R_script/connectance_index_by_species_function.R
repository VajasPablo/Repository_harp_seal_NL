
# Function to calculate connectance index by species

connectance_index_by_species_function <- function(EwE_period, data_raw, select_species) {
 
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
  
  Df_clean <- Df_clean[-1, -1]
  
  Species_names <- subset(Df_clean$X2, !(Df_clean$X2 %in% c("Small Phytoplankton", "Large Phytoplankton", "Detritus", "Import", "Sum", "(1 - Sum)")))
  Species_names <- c("Prey/Predator", Species_names)
  
  Df_clean <- Df_clean %>%
    rename_with(~ Species_names, everything())
  
  # Number of paths
  path_count <- Df_clean %>% 
    filter(!(`Prey/Predator` %in% c("Import", "Sum", "(1 - Sum)"))) %>% 
    summarise_all(~ sum(. > 0)) %>%
    pivot_longer(cols = everything(),
                 names_to = "Species_Group",
                 values_to = "Number_interaction") %>%
    filter(!(Species_Group %in% "Prey/Predator"))
  
  Total_number_paths <- sum(path_count$Number_interaction)
  
  # Connectance index calculation
  
  if (select_species == "Cod") {
    
    species_row <- Df_clean %>% 
      filter(`Prey/Predator` %in% c("Cod> 35 cm", "Cod<= 35 cm")) %>%
      select(-"Prey/Predator") %>% 
      summarise_all(~ sum(. > 0)) %>% 
      mutate(`Prey/Predator` = "Cod") %>% 
      mutate(across(-"Prey/Predator", as.numeric)) %>%
      select(-"Prey/Predator") %>%
      pivot_longer(cols = everything(),
                   names_to = "EwE_groups",
                   values_to = "Number_interaction") %>% 
      mutate(Number_interaction = ifelse(Number_interaction > 0, 1, 0)) %>%
      summarise_all(~ sum(. > 0)) %>%
      pull()
    
    species_column <- Df_clean %>% 
      filter(!(`Prey/Predator` %in% c("Import", "Sum", "(1 - Sum)"))) %>% 
      select(c("Cod> 35 cm", "Cod<= 35 cm")) %>%
      mutate_all(as.numeric) %>%
      mutate(Cod = `Cod> 35 cm` + `Cod<= 35 cm`) %>%
      summarise_all(~ sum(. > 0)) %>%
      pull()
    
  } else {
    
    species_row <- Df_clean %>% 
      filter(`Prey/Predator` %in% select_species) %>%
      mutate_at(vars(-"Prey/Predator"), as.numeric) %>% 
      select(-"Prey/Predator") %>% 
      pivot_longer(cols = everything(),
                   names_to = "EwE_groups",
                   values_to = "Number_interaction") %>%
      summarise_all(~ sum(. > 0)) %>% select(Number_interaction) %>%
      pull()
    
    species_column <- Df_clean %>% 
      filter(!(`Prey/Predator` %in% c("Import", "Sum", "(1 - Sum)"))) %>% 
      select(all_of(select_species)) %>%
      mutate(across(where(is.numeric), as.numeric)) %>%
      summarise_all(~ sum(. > 0)) %>%
      pull()
    
  }
  
  Connectance_species <- (species_row + species_column) / Total_number_paths
  
  assign(paste0(EwE_period, "_Connectance_Index_", select_species), Connectance_species, envir = .GlobalEnv)
  
  for_print <- c(select_species, Connectance_species)
  print(for_print)
  
}
