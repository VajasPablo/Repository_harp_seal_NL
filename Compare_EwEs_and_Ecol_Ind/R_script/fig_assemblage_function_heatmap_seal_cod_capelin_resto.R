
fig_assemblage_function_heatmap_seal_cod_capelin_resto <- function(){
  
  
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K.rds"))
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO.rds"))
  
  ### 2020 2J3K Cod ###
  
  heat_2020_2J3K_cod <- heatmap_multispecific_biomass_restoration_function_ajust(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K,
                                                                           species_number = c(12, 13),
                                                                           species_one_fixe = "seal", 
                                                                           species_two_fixe = "cod", 
                                                                           species_one = "cod",
                                                                           species_two = "seal",
                                                                           target_species = "Cod",
                                                                           period = "EwE 2020 2J3K",
                                                                           hist_val = 4.534)
  
  
  ### 2020 3LNO Cod ###
  
  heat_2020_3LNO_cod <- heatmap_multispecific_biomass_restoration_function_ajust(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO,
                                                                           species_number = c(12, 13),
                                                                           species_one_fixe = "seal", 
                                                                           species_two_fixe = "cod", 
                                                                           species_one = "cod",
                                                                           species_two = "seal",
                                                                           target_species = "Cod",
                                                                           period = "EwE 2020 3LNO",
                                                                           hist_val = 4.534)
  
  Df_merged_seal_vs_capelin_biomass_final_EwE_2020_2J3K <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_capelin_biomass_final_EwE_2020_2J3K.rds"))
  Df_merged_seal_vs_capelin_biomass_final_EwE_2020_3LNO <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_capelin_biomass_final_EwE_2020_3LNO.rds"))
  
  ### 2020 2J3K capelin ###
  
  heat_2020_2J3K_capelin <- heatmap_multispecific_biomass_restoration_function_ajust(data = Df_merged_seal_vs_capelin_biomass_final_EwE_2020_2J3K,
                                                                               species_number = c(12, 13),
                                                                               species_one_fixe = "seal", 
                                                                               species_two_fixe = "capelin", 
                                                                               species_one = "seal",
                                                                               species_two = "capelin",
                                                                               target_species = "Cod",
                                                                               period = "EwE 2020 2J3K",
                                                                               hist_val = 4.534)
  
  
  ### 2020 3LNO capelin ###
  
  heat_2020_3LNO_capelin <- heatmap_multispecific_biomass_restoration_function_ajust(data = Df_merged_seal_vs_capelin_biomass_final_EwE_2020_3LNO,
                                                                               species_number = c(12, 13),
                                                                               species_one_fixe = "seal", 
                                                                               species_two_fixe = "capelin", 
                                                                               species_one = "seal",
                                                                               species_two = "capelin",
                                                                               target_species = "Cod",
                                                                               period = "EwE 2020 3LNO",
                                                                               hist_val = 4.534)
  ### grid arrange ###
  
  plots <- list(heat_2020_2J3K_cod + ggtitle("EwE 2020 2J3K"), 
                heat_2020_2J3K_capelin + ggtitle("EwE 2020 2J3K"), 
                heat_2020_3LNO_cod + ggtitle("EwE 2020 3LNO"), 
                heat_2020_3LNO_capelin + ggtitle("EwE 2020 3LNO"))
  
  # Utilisez ggarrange pour combiner les graphiques
  combined_plot <- suppressWarnings(ggarrange(plotlist = plots, ncol = 2, nrow = 2,
                             common.legend = TRUE, legend = "right"))
  
  suppressWarnings(print(combined_plot))
  
}