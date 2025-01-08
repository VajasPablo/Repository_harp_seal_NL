
### All key figures ###

fig_assemblage_function_heatmap_seal_cod_resto_sensitivity <- function() {
  

  ### 2020 rds ###
  
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K.rds"))
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO.rds"))
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO.rds"))
  
  ### 2020 2J3K Cod ###
  
  heat_2020_2J3K_cod <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K,
                                                                           species_number = c(12, 13),
                                                                           species_one_fixe = "seal", 
                                                                           species_two_fixe = "cod", 
                                                                           species_one = "cod",
                                                                           species_two = "seal",
                                                                           target_species = "Cod",
                                                                           period = "EwE 2020 2J3K",
                                                                           hist_val = 4.534)
  
  
  ### 2020 3LNO Cod ###
  
  heat_2020_3LNO_cod <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO,
                                                                           species_number = c(12, 13),
                                                                           species_one_fixe = "seal", 
                                                                           species_two_fixe = "cod", 
                                                                           species_one = "cod",
                                                                           species_two = "seal",
                                                                           target_species = "Cod",
                                                                           period = "EwE 2020 3LNO",
                                                                           hist_val = 4.534)
  
  
  ### 2020 merged Cod ###
  
  heat_2020_merged_cod <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO,
                                                                             species_number = c(12, 13),
                                                                             species_one_fixe = "seal", 
                                                                             species_two_fixe = "cod", 
                                                                             species_one = "cod",
                                                                             species_two = "seal",
                                                                             target_species = "Cod",
                                                                             period = "EwE 2020 2J3K 3LNO",
                                                                             hist_val = 4.534)
  
  ### 2020 sensitivity rds ###
  
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K_80_20 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K_80_20.rds"))
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO_80_20 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO_80_20.rds"))
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20.rds"))
  
  ### 2020 2J3K Cod ###
  
  heat_2020_2J3K_cod_80_20 <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K_80_20,
                                                                           species_number = c(12, 13),
                                                                           species_one_fixe = "seal", 
                                                                           species_two_fixe = "cod", 
                                                                           species_one = "cod",
                                                                           species_two = "seal",
                                                                           target_species = "Cod",
                                                                           period = "EwE 2020 2J3K Sensitivity",
                                                                           hist_val = 4.534)
  
  
  ### 2020 3LNO Cod ###
  
  heat_2020_3LNO_cod_80_20 <- heatmap_multispecific_biomass_restoration_function_3LNO_Sens(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO_80_20,
                                                                           species_number = c(12, 13),
                                                                           species_one_fixe = "seal", 
                                                                           species_two_fixe = "cod", 
                                                                           species_one = "cod",
                                                                           species_two = "seal",
                                                                           target_species = "Cod",
                                                                           period = "EwE 2020 3LNO Sensitivity",
                                                                           hist_val = 4.534)
  
  
  ### 2020 merged Cod ###
  
  heat_2020_merged_cod_80_20 <- heatmap_multispecific_biomass_restoration_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20,
                                                                             species_number = c(12, 13),
                                                                             species_one_fixe = "seal", 
                                                                             species_two_fixe = "cod", 
                                                                             species_one = "cod",
                                                                             species_two = "seal",
                                                                             target_species = "Cod",
                                                                             period = "EwE 2020 2J3K 3LNO Sensitivity",
                                                                             hist_val = 4.534)
  
  ### grid arrange ###
  
  suppressWarnings(grid.arrange(  
    heat_2020_2J3K_cod,  
    heat_2020_3LNO_cod, 
    heat_2020_merged_cod,
    heat_2020_2J3K_cod_80_20,  
    heat_2020_3LNO_cod_80_20, 
    heat_2020_merged_cod_80_20,
    ncol = 3))
}
