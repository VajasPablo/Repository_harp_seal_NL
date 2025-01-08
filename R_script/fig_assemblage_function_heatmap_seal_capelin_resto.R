
### All key figures ###

fig_assemblage_function_heatmap_seal_capelin_resto <- function() {
  
  ### 2013 rds ###
  
  Df_merged_seal_vs_capelin_biomass_final_EwE_2013 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_capelin_biomass_final_EwE_2013.rds"))
  
  ### 2013 capelin ###
  
  heat_2013_capelin <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_capelin_biomass_final_EwE_2013,
                                                                      species_number = c(13, 14),
                                                                      species_one_fixe = "seal", 
                                                                      species_two_fixe = "capelin", 
                                                                      species_one = "seal",
                                                                      species_two = "capelin",
                                                                      target_species = "Cod",
                                                                      period = "EwE 2013",
                                                                      hist_val = 4.534)
  
  ### 2020 rds ###
  
  Df_merged_seal_vs_capelin_biomass_final_EwE_2020_2J3K <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_capelin_biomass_final_EwE_2020_2J3K.rds"))
  Df_merged_seal_vs_capelin_biomass_final_EwE_2020_3LNO <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_capelin_biomass_final_EwE_2020_3LNO.rds"))
  Df_merged_seal_vs_capelin_biomass_final_EwE_2020_merged_2J3K_3LNO <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_capelin_biomass_final_EwE_2020_merged_2J3K_3LNO.rds"))
  
  ### 2020 2J3K capelin ###
  
  heat_2020_2J3K_capelin <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_capelin_biomass_final_EwE_2020_2J3K,
                                                                           species_number = c(12, 13),
                                                                           species_one_fixe = "seal", 
                                                                           species_two_fixe = "capelin", 
                                                                           species_one = "seal",
                                                                           species_two = "capelin",
                                                                           target_species = "Cod",
                                                                           period = "EwE 2020 2J3K",
                                                                           hist_val = 4.534)
  
  
  ### 2020 3LNO capelin ###
  
  heat_2020_3LNO_capelin <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_capelin_biomass_final_EwE_2020_3LNO,
                                                                           species_number = c(12, 13),
                                                                           species_one_fixe = "seal", 
                                                                           species_two_fixe = "capelin", 
                                                                           species_one = "seal",
                                                                           species_two = "capelin",
                                                                           target_species = "Cod",
                                                                           period = "EwE 2020 3LNO",
                                                                           hist_val = 4.534)
  
  
  ### 2020 merged capelin ###
  
  heat_2020_merged_capelin <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_capelin_biomass_final_EwE_2020_merged_2J3K_3LNO,
                                                                             species_number = c(12, 13),
                                                                             species_one_fixe = "seal", 
                                                                             species_two_fixe = "capelin", 
                                                                             species_one = "seal",
                                                                             species_two = "capelin",
                                                                             target_species = "Cod",
                                                                             period = "EwE 2020 2J3K 3LNO",
                                                                             hist_val = 4.534)
  
  ### grid arrange ###
  
  suppressWarnings(grid.arrange(  
    heat_2013_capelin, 
    heat_2020_2J3K_capelin,  
    heat_2020_3LNO_capelin, 
    heat_2020_merged_capelin,
    ncol = 2))
}

