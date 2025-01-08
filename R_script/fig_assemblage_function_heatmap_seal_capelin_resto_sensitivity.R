
### All key figures ###

fig_assemblage_function_heatmap_seal_capelin_resto_sensitivity <- function() {
  
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
  
  ### 2020 sensitivity rds ###
  
  Df_merged_seal_vs_capelin_biomass_final_EwE_2020_2J3K_80_20 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_capelin_biomass_final_EwE_2020_2J3K_80_20.rds"))
  Df_merged_seal_vs_capelin_biomass_final_EwE_2020_3LNO_80_20 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_capelin_biomass_final_EwE_2020_3LNO_80_20.rds"))
  Df_merged_seal_vs_capelin_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_capelin_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20.rds"))
  
  ### 2020 2J3K capelin ###
  
  heat_2020_2J3K_capelin_80_20 <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_capelin_biomass_final_EwE_2020_2J3K,
                                                                               species_number = c(12, 13),
                                                                               species_one_fixe = "seal", 
                                                                               species_two_fixe = "capelin", 
                                                                               species_one = "seal",
                                                                               species_two = "capelin",
                                                                               target_species = "Cod",
                                                                               period = "EwE 2020 2J3K Sensitivity",
                                                                               hist_val = 4.534)
  
  
  ### 2020 3LNO capelin ###
  
  heat_2020_3LNO_capelin_80_20 <- heatmap_multispecific_biomass_restoration_function_3LNO_Sens(data = Df_merged_seal_vs_capelin_biomass_final_EwE_2020_3LNO,
                                                                               species_number = c(12, 13),
                                                                               species_one_fixe = "seal", 
                                                                               species_two_fixe = "capelin", 
                                                                               species_one = "seal",
                                                                               species_two = "capelin",
                                                                               target_species = "Cod",
                                                                               period = "EwE 2020 3LNO Sensitivity",
                                                                               hist_val = 4.534)
  
  
  ### 2020 merged capelin ###
  
  heat_2020_merged_capelin_80_20 <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_capelin_biomass_final_EwE_2020_merged_2J3K_3LNO,
                                                                                 species_number = c(12, 13),
                                                                                 species_one_fixe = "seal", 
                                                                                 species_two_fixe = "capelin", 
                                                                                 species_one = "seal",
                                                                                 species_two = "capelin",
                                                                                 target_species = "Cod",
                                                                                 period = "EwE 2020 2J3K 3LNO Sensitivity",
                                                                                 hist_val = 4.534)

  ### grid arrange ###
  
  suppressWarnings(grid.arrange(  
    heat_2020_2J3K_capelin,  
    heat_2020_3LNO_capelin, 
    heat_2020_merged_capelin,
    heat_2020_2J3K_capelin_80_20,  
    heat_2020_3LNO_capelin_80_20, 
    heat_2020_merged_capelin_80_20,
    ncol = 3))
}

