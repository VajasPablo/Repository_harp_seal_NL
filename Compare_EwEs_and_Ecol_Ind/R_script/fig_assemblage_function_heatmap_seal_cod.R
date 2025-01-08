
### All key figures ###

fig_assemblage_function_heatmap_seal_cod <- function(species){
  
  ### 1985 rds ###
  
  Df_merged_seal_vs_cod_biomass_final_EwE_1985 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_1985.rds"))
  
  
  ### 1985 Cod ###
  
  heat_1985_cod <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_1985,
                                                                 species_number = c(13, 14),
                                                                 species_one_fixe = "seal", 
                                                                 species_two_fixe = "cod", 
                                                                 species_one = "cod",
                                                                 species_two = "seal",
                                                                 target_species = "Cod",
                                                                 sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                 sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                 period = "EwE 1985",
                                                                 obs_dens = "4.52")
  
  ### 1985 Harp Seal  ###
  
  heat_1985_seal <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_1985,
                                                                  species_number = 6,
                                                                  species_one_fixe = "seal", 
                                                                  species_two_fixe = "cod", 
                                                                  species_one = "seal",
                                                                  species_two = "cod",
                                                                  target_species = "seal",
                                                                  sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                  sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                  period = "EwE 1985",
                                                                  obs_dens = "0.213")
  
  ### 1985 Capelin  ###
  
  heat_1985_capelin <-   heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_1985,
                                                                       species_number = 32,
                                                                       species_one_fixe = "seal", 
                                                                       species_two_fixe = "cod", 
                                                                       species_one = "seal",
                                                                       species_two = "cod",
                                                                       target_species = "Capelin",
                                                                       sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                       sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                       period = "EwE 1985",
                                                                       obs_dens = "13.77")
  
  ### 2013 rds ###
  
  Df_merged_seal_vs_cod_biomass_final_EwE_2013 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2013.rds"))
  
  ### 2013 Cod ###
  
  heat_2013_cod <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2013,
                                                                 species_number = c(13, 14),
                                                                 species_one_fixe = "seal", 
                                                                 species_two_fixe = "cod", 
                                                                 species_one = "cod",
                                                                 species_two = "seal",
                                                                 target_species = "Cod",
                                                                 period = "EwE 2013",
                                                                 sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                 sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                 obs_dens = "0.8")
  
  ### 2013 Harp Seal  ###
  
  heat_2013_seal <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2013,
                                                                  species_number = 6,
                                                                  species_one_fixe = "seal", 
                                                                  species_two_fixe = "cod", 
                                                                  species_one = "seal",
                                                                  species_two = "cod",
                                                                  target_species = "seal",
                                                                  period = "EwE 2013",
                                                                  sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                  sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                  obs_dens = "0.266")
  
  ### 2013 Capelin  ###
  
  heat_2013_capelin <-   heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2013,
                                                                       species_number = 32,
                                                                       species_one_fixe = "seal", 
                                                                       species_two_fixe = "cod", 
                                                                       species_one = "seal",
                                                                       species_two = "cod",
                                                                       target_species = "Capelin",
                                                                       period = "EwE 2013",
                                                                       sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                       sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                       obs_dens = "4.97")
  
  ### 2020 rds ###
  
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K.rds"))
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO.rds"))
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO.rds"))
  
  ### 2020 2J3K Cod ###
  
  heat_2020_2J3K_cod <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K,
                                                                      species_number = c(12, 13),
                                                                      species_one_fixe = "seal", 
                                                                      species_two_fixe = "cod", 
                                                                      species_one = "cod",
                                                                      species_two = "seal",
                                                                      target_species = "Cod",
                                                                      sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                      sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                      period = "EwE 2020 2J3K",
                                                                      obs_dens = "2.38")
  
  ### 2020 2J3K Harp Seal  ###
  
  heat_2020_2J3K_seal <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K,
                                                                       species_number = 5,
                                                                       species_one_fixe = "seal", 
                                                                       species_two_fixe = "cod", 
                                                                       species_one = "seal",
                                                                       species_two = "cod",
                                                                       target_species = "seal",
                                                                       sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                       sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                       period = "EwE 2020 2J3K",
                                                                       obs_dens = "0.283")
  
  ### 2020 2J3K capelin  ###
  
  heat_2020_2J3K_capelin <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K,
                                                                          species_number = 28,
                                                                          species_one_fixe = "seal", 
                                                                          species_two_fixe = "cod", 
                                                                          species_one = "seal",
                                                                          species_two = "cod",
                                                                          target_species = "Capelin",
                                                                          sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                          sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                          period = "EwE 2020 2J3K",
                                                                          obs_dens = "3.05")
  
  ### 2020 3LNO Cod ###
  
  heat_2020_3LNO_cod <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO,
                                                                      species_number = c(12, 13),
                                                                      species_one_fixe = "seal", 
                                                                      species_two_fixe = "cod", 
                                                                      species_one = "cod",
                                                                      species_two = "seal",
                                                                      target_species = "Cod",
                                                                      sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                      sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                      period = "EwE 2020 3LNO",
                                                                      obs_dens = "0.62")
  
  ### 2020 3LNO Harp Seal  ###
  
  heat_2020_3LNO_seal <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO,
                                                                       species_number = 5,
                                                                       species_one_fixe = "seal", 
                                                                       species_two_fixe = "cod", 
                                                                       species_one = "seal",
                                                                       species_two = "cod",
                                                                       target_species = "seal",
                                                                       sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                       sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                       period = "EwE 2020 3LNO",
                                                                       obs_dens = "0.261")
  
  ### 2020 3LNO Capelin  ###
  
  heat_2020_3LNO_capelin <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO,
                                                                          species_number = 31,
                                                                          species_one_fixe = "seal", 
                                                                          species_two_fixe = "cod", 
                                                                          species_one = "seal",
                                                                          species_two = "cod",
                                                                          target_species = "Capelin",
                                                                          sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                          sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                          period = "EwE 2020 3LNO",
                                                                          obs_dens = "3.50")
  
  ### 2020 merged Cod ###
  
  heat_2020_merged_cod <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO,
                                                                        species_number = c(12, 13),
                                                                        species_one_fixe = "seal", 
                                                                        species_two_fixe = "cod", 
                                                                        species_one = "cod",
                                                                        species_two = "seal",
                                                                        target_species = "Cod",
                                                                        sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                        sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                        period = "EwE 2020 2J3K 3LNO",
                                                                        obs_dens = "1.46")
  
  ### 2020 merged Harp Seal  ###
  
  heat_2020_merged_seal <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO,
                                                                         species_number = 5,
                                                                         species_one_fixe = "seal", 
                                                                         species_two_fixe = "cod", 
                                                                         species_one = "seal",
                                                                         species_two = "cod",
                                                                         target_species = "seal",
                                                                         sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                         sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                         period = "EwE 2020 2J3K 3LNO",
                                                                         obs_dens = "0.271")
  
  ### 2020 merged Capelin  ###
  
  heat_2020_merged_capelin <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO,
                                                                            species_number = 31,
                                                                            species_one_fixe = "seal", 
                                                                            species_two_fixe = "cod", 
                                                                            species_one = "seal",
                                                                            species_two = "cod",
                                                                            target_species = "Capelin",
                                                                            sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                            sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                            period = "EwE 2020 2J3K 3LNO",
                                                                            obs_dens = "3.284")
  
  
  ### 2020 sensitivity rds ###
  
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K_80_20 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K_80_20.rds"))
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO_80_20 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO_80_20.rds"))
  Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20 <- readRDS(file = here("Rds_import_adapted_data", "Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20.rds"))
  
  ### 2020 2J3K Cod ###
  
  heat_2020_2J3K_cod_80_20 <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K_80_20,
                                                                            species_number = c(12, 13),
                                                                            species_one_fixe = "seal", 
                                                                            species_two_fixe = "cod", 
                                                                            species_one = "cod",
                                                                            species_two = "seal",
                                                                            target_species = "Cod",
                                                                            sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                            sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                            period = "EwE 2020 2J3K Sensitivity",
                                                                            obs_dens = "2.38")
  
  ### 2020 2J3K Harp Seal  ###
  
  heat_2020_2J3K_seal_80_20 <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K_80_20,
                                                                             species_number = 5,
                                                                             species_one_fixe = "seal", 
                                                                             species_two_fixe = "cod", 
                                                                             species_one = "seal",
                                                                             species_two = "cod",
                                                                             target_species = "seal",
                                                                             sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                             sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                             period = "EwE 2020 2J3K Sensitivity",
                                                                             obs_dens = "0.4524")
  
  ### 2020 2J3K capelin  ###
  
  heat_2020_2J3K_capelin_80_20 <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_2J3K_80_20,
                                                                                species_number = 28,
                                                                                species_one_fixe = "seal", 
                                                                                species_two_fixe = "cod", 
                                                                                species_one = "seal",
                                                                                species_two = "cod",
                                                                                target_species = "Capelin",
                                                                                sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                                sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                                period = "EwE 2020 2J3K Sensitivity",
                                                                                obs_dens = "3.05")
  ### 2020 3LNO Cod ###
  
  heat_2020_3LNO_cod_80_20 <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO_80_20,
                                                                            species_number = c(12, 13),
                                                                            species_one_fixe = "seal", 
                                                                            species_two_fixe = "cod", 
                                                                            species_one = "cod",
                                                                            species_two = "seal",
                                                                            target_species = "Cod",
                                                                            sp2_lvl = c("0", "25", "obs", "50", "75", "99"),
                                                                            sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                            period = "EwE 2020 3LNO Sensitivity",
                                                                            obs_dens = "0.62")
  
  ### 2020 3LNO Harp Seal  ###
  
  heat_2020_3LNO_seal_80_20 <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO_80_20,
                                                                             species_number = 5,
                                                                             species_one_fixe = "seal", 
                                                                             species_two_fixe = "cod", 
                                                                             species_one = "seal",
                                                                             species_two = "cod",
                                                                             target_species = "seal",
                                                                             sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                             sp1_lvl = c("99", "75", "50", "obs", "25", "0"),
                                                                             period = "EwE 2020 3LNO Sensitivity",
                                                                             obs_dens = "0.1044")
  
  ### 2020 3LNO Capelin  ###
  
  heat_2020_3LNO_capelin_80_20 <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_3LNO_80_20,
                                                                                species_number = 31,
                                                                                species_one_fixe = "seal", 
                                                                                species_two_fixe = "cod", 
                                                                                species_one = "seal",
                                                                                species_two = "cod",
                                                                                target_species = "Capelin",
                                                                                sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                                sp1_lvl = c("99", "75", "50", "obs", "25", "0"),
                                                                                period = "EwE 2020 3LNO Sensitivity",
                                                                                obs_dens = "3.50")
  ### 2020 merged Cod ###
  
  heat_2020_merged_cod_80_20 <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20,
                                                                              species_number = c(12, 13),
                                                                              species_one_fixe = "seal", 
                                                                              species_two_fixe = "cod", 
                                                                              species_one = "cod",
                                                                              species_two = "seal",
                                                                              target_species = "Cod",
                                                                              sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                              sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                              period = "EwE 2020 2J3K 3LNO Sensitivity",
                                                                              obs_dens = "1.46")
  
  ### 2020 merged Harp Seal  ###
  
  heat_2020_merged_seal_80_20 <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20,
                                                                               species_number = 5,
                                                                               species_one_fixe = "seal", 
                                                                               species_two_fixe = "cod", 
                                                                               species_one = "seal",
                                                                               species_two = "cod",
                                                                               target_species = "seal",
                                                                               sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                               sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                               period = "EwE 2020 2J3K 3LNO Sensitivity",
                                                                               obs_dens = "0.271")
  
  ### 2020 merged Capelin  ###
  
  heat_2020_merged_capelin_80_20 <- heatmap_multispecific_biomass_growth_function(data = Df_merged_seal_vs_cod_biomass_final_EwE_2020_merged_2J3K_3LNO_80_20,
                                                                                  species_number = 31,
                                                                                  species_one_fixe = "seal", 
                                                                                  species_two_fixe = "cod", 
                                                                                  species_one = "seal",
                                                                                  species_two = "cod",
                                                                                  target_species = "Capelin",
                                                                                  sp2_lvl = c("0", "obs", "25", "50", "75", "99"),
                                                                                  sp1_lvl = c("99", "75", "50", "25", "obs", "0"),
                                                                                  period = "EwE 2020 2J3K 3LNO Sensitivity",
                                                                                  obs_dens = "3.284")
  
  
  
  ### grid arrange ###
  
  if (species == "cod") {
    
    suppressWarnings(grid.arrange(heat_1985_cod,  
                                  heat_2013_cod, 
                                  heat_2020_2J3K_cod,  
                                  heat_2020_3LNO_cod, 
                                  heat_2020_merged_cod,
                                  ncol = 2))
    
  } else if (species == "seal") {
    
    suppressWarnings(grid.arrange(heat_1985_seal, 
                                  heat_2013_seal,
                                  heat_2020_2J3K_seal, 
                                  heat_2020_3LNO_seal,
                                  heat_2020_merged_seal,
                                  ncol = 2))
    
  } else if (species == "capelin") {
    
    suppressWarnings(grid.arrange(heat_1985_capelin, 
                                  heat_2013_capelin,
                                  heat_2020_2J3K_capelin, 
                                  heat_2020_3LNO_capelin,
                                  heat_2020_merged_capelin,
                                  ncol = 2))
    
  } else if (species == "cod_sensitivity") {
    
    suppressWarnings(grid.arrange(heat_2020_2J3K_cod,  
                                  heat_2020_3LNO_cod, 
                                  heat_2020_merged_cod,
                                  heat_2020_2J3K_cod_80_20,  
                                  heat_2020_3LNO_cod_80_20, 
                                  heat_2020_merged_cod_80_20,
                                  ncol = 3))
    
  } else if (species == "seal_sensitivity") {
    
    suppressWarnings(grid.arrange(heat_2020_2J3K_seal, 
                                  heat_2020_3LNO_seal,
                                  heat_2020_merged_seal,
                                  heat_2020_2J3K_seal_80_20, 
                                  heat_2020_3LNO_seal_80_20,
                                  heat_2020_merged_seal_80_20,
                                  ncol = 3))
    
  } else if (species == "capelin_sensitivity") {
    
    suppressWarnings(grid.arrange(heat_2020_2J3K_capelin, 
                                  heat_2020_3LNO_capelin,
                                  heat_2020_merged_capelin,
                                  heat_2020_2J3K_capelin_80_20, 
                                  heat_2020_3LNO_capelin_80_20,
                                  heat_2020_merged_capelin_80_20,
                                  ncol = 3))
  }
}
