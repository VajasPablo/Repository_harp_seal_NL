
### All key figures ###

key_fig_assemblage_function_LoR <- function(species){
  
  ### 2020 50/50 ###

  # cod 2J3K

  cod_2020_2J3K_50_50_key1 <- key_fig_function_LoR(df_species = cod_Bsim_GrpImpact_EwE_2020_2J3K,
                       df_species_catch= cod_Bsim_catch_EwE_2020_2J3K,
                       obs_lod_data= 8,
                       x_axis_species= "Cod",
                       unique_title ="Cod LoR - EwE 2020 - 2J3K",
                       legend = "none")
  
  cod_2020_2J3K_50_50_key2 <- key_fig_function_LoR_web(LoR_order = c(1, 10, 20, 30, 40, 50, 60,"obs", 70, 80, 90, 100),
                           df_species = cod_web_Bsim_GrpImpact_EwE_2020_2J3K,
                           obs_LoR_data = 8,
                           x_axis_species = "Cod",
                           unique_title ="Cod LoR - EwE 2020 - 2J3K",
                           legend = "none")
  
  # cod 3LNO
  
  cod_2020_3LNO_50_50_key1 <- key_fig_function_LoR(df_species = cod_Bsim_GrpImpact_EwE_2020_3LNO,
                                                   df_species_catch= cod_Bsim_catch_EwE_2020_3LNO,
                                                   obs_lod_data= 3,
                                                   x_axis_species= "Cod",
                                                   unique_title ="Cod LoR - EwE 2020 - 3LNO",
                                                   legend = "none")
  
  cod_2020_3LNO_50_50_key2 <- key_fig_function_LoR_web(LoR_order = c(1, 10, "obs", 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                                                       df_species = cod_web_Bsim_GrpImpact_EwE_2020_3LNO,
                                                       obs_LoR_data = 3,
                                                       x_axis_species = "Cod",
                                                       unique_title ="Cod LoR - EwE 2020 - 3LNO",
                                                       legend = "none")
  
  # cod merged
  
  cod_2020_2J3K_3LNO_50_50_key1 <- key_fig_function_LoR_standardized(df_species = cod_Bsim_GrpImpact_EwE_2020_merged_2J3K_3LNO,
                                    df_species_catch= cod_Bsim_catch_EwE_2020_merged_2J3K_3LNO,
                                    x_axis_species = "Cod",
                                    unique_title ="Cod LoR - EwE 2020 - 2J3K-3LNO",
                                    legend = "none")

  cod_2020_2J3K_3LNO_50_50_key2 <- key_fig_function_LoR_web_standardized(df_species = cod_web_Bsim_GrpImpact_EwE_2020_merged_2J3K_3LNO,
                                        x_axis_species = "Cod",
                                        unique_title ="Cod LoR - EwE 2020 - 2J3K-3LNO",
                                        legend = "none")
  
  # capelin 2J3K
  
  capelin_2020_2J3K_50_50_key1 <- key_fig_function_LoR(df_species = capelin_Bsim_GrpImpact_EwE_2020_2J3K,
                       df_species_catch= capelin_Bsim_catch_EwE_2020_2J3K,
                       obs_lod_data= 4,
                       x_axis_species= "Capelin",
                       unique_title ="Capelin LoR - EwE 2020 - 2J3K",
                       legend = "none")
  
  capelin_2020_2J3K_50_50_key2 <- key_fig_function_LoR_web(LoR_order = c(1, 10, 20, "obs", 30, 40, 50, 60, 70, 80, 90, 100),
                           df_species = capelin_web_Bsim_GrpImpact_EwE_2020_2J3K,
                           obs_LoR_data = 4,
                           x_axis_species = "Capelin",
                           unique_title ="Capelin LoR - EwE 2020 - 2J3K",
                           legend = "none")
  
  # capelin 3LNO

capelin_2020_3LNO_50_50_key1 <- key_fig_function_LoR(df_species = capelin_Bsim_GrpImpact_EwE_2020_3LNO,
                                                     df_species_catch= capelin_Bsim_catch_EwE_2020_3LNO,
                                                     obs_lod_data= 4,
                                                     x_axis_species= "Capelin",
                                                     unique_title ="Capelin LoR - EwE 2020 - 3LNO",
                                                     legend = "none")

capelin_2020_3LNO_50_50_key2 <- key_fig_function_LoR_web(LoR_order = c(1, 10, 20, "obs", 30, 40, 50, 60, 70, 80, 90, 100),
                                                         df_species = capelin_web_Bsim_GrpImpact_EwE_2020_3LNO,
                                                         obs_LoR_data = 4,
                                                         x_axis_species = "Capelin",
                                                         unique_title ="Capelin LoR - EwE 2020 - 3LNO",
                                                         legend = "none")
  
  # capelin merged
  
capelin_2020_2J3K_3LNO_50_50_key1 <- key_fig_function_LoR_standardized(df_species = capelin_Bsim_GrpImpact_EwE_2020_merged_2J3K_3LNO,
                                                                   df_species_catch= capelin_Bsim_catch_EwE_2020_merged_2J3K_3LNO,
                                                                   x_axis_species = "Capelin",
                                                                   unique_title ="Capelin LoR - EwE 2020 - 2J3K-3LNO",
                                                                   legend = "none")

capelin_2020_2J3K_3LNO_50_50_key2 <- key_fig_function_LoR_web_standardized(df_species = capelin_web_Bsim_GrpImpact_EwE_2020_merged_2J3K_3LNO,
                                                                       x_axis_species = "Capelin",
                                                                       unique_title ="Capeling LoR - EwE 2020 - 2J3K-3LNO",
                                                                       legend = "none")
### 2020 Sensitivity ###

# cod 2J3K

cod_2020_80_20_2J3K_50_50_key1 <- key_fig_function_LoR(df_species = cod_Bsim_GrpImpact_EwE_2020_80_20_2J3K,
                                                       df_species_catch= cod_Bsim_catch_EwE_2020_80_20_2J3K,
                                                       obs_lod_data= 8,
                                                       x_axis_species= "Cod",
                                                       unique_title ="Cod LoR - EwE 2020 - 2J3K - Sensitivity",
                                                       legend = "none")

cod_2020_80_20_2J3K_50_50_key2 <- key_fig_function_LoR_web(LoR_order = c(1, 10, 20, 30, 40, 50, 60,"obs", 70, 80, 90, 100), 
                                                           df_species = cod_web_Bsim_GrpImpact_EwE_2020_80_20_2J3K,
                                                           obs_LoR_data = 8,
                                                           x_axis_species = "Cod",
                                                           unique_title ="Cod LoR - EwE 2020 - 2J3K - Sensitivity",
                                                           legend = "none")

# cod 3LNO

cod_2020_80_20_3LNO_50_50_key1 <- key_fig_function_LoR(df_species = cod_Bsim_GrpImpact_EwE_2020_80_20_3LNO,
                                                       df_species_catch= cod_Bsim_catch_EwE_2020_80_20_3LNO,
                                                       obs_lod_data= 3,
                                                       x_axis_species= "Cod",
                                                       unique_title ="Cod LoR - EwE 2020 - 3LNO - Sensitivity",
                                                       legend = "none")

cod_2020_80_20_3LNO_50_50_key2 <- key_fig_function_LoR_web(LoR_order = c(1, 10, "obs", 20, 30, 40, 50, 60, 70, 80, 90, 100),
                                                           df_species = cod_web_Bsim_GrpImpact_EwE_2020_80_20_3LNO,
                                                           obs_LoR_data = 3,
                                                           x_axis_species = "Cod",
                                                           unique_title ="Cod LoR - EwE 2020 - 3LNO - Sensitivity",
                                                           legend = "none")

# cod merged

cod_2020_80_20_2J3K_3LNO_50_50_key1 <- key_fig_function_LoR_standardized(df_species = cod_Bsim_GrpImpact_EwE_2020_80_20_merged_2J3K_3LNO,
                                                                         df_species_catch= cod_Bsim_catch_EwE_2020_80_20_merged_2J3K_3LNO,
                                                                         x_axis_species = "Cod",
                                                                         unique_title ="Cod LoR - EwE 2020 - 2J3K-3LNO - Sensitivity",
                                                                         legend = "none")

cod_2020_80_20_2J3K_3LNO_50_50_key2 <- key_fig_function_LoR_web_standardized(df_species = cod_web_Bsim_GrpImpact_EwE_2020_80_20_merged_2J3K_3LNO,
                                                                             x_axis_species = "Cod",
                                                                             unique_title ="Cod LoR - EwE 2020 - 2J3K-3LNO - Sensitivity",
                                                                             legend = "none")

# capelin 2J3K

capelin_2020_80_20_2J3K_50_50_key1 <- key_fig_function_LoR(df_species = capelin_Bsim_GrpImpact_EwE_2020_80_20_2J3K,
                                                           df_species_catch= capelin_Bsim_catch_EwE_2020_80_20_2J3K,
                                                           obs_lod_data= 4,
                                                           x_axis_species= "Capelin",
                                                           unique_title ="Capelin LoR - EwE 2020 - 2J3K - Sensitivity",
                                                           legend = "none")

capelin_2020_80_20_2J3K_50_50_key2 <- key_fig_function_LoR_web(LoR_order = c(1, 10, 20, "obs", 30, 40, 50, 60, 70, 80, 90, 100),  
                                                               df_species = capelin_web_Bsim_GrpImpact_EwE_2020_80_20_2J3K,
                                                               obs_LoR_data = 4,
                                                               x_axis_species = "Capelin",
                                                               unique_title ="Capelin LoR - EwE 2020 - 2J3K - Sensitivity",
                                                               legend = "none")

# capelin 3LNO

capelin_2020_80_20_3LNO_50_50_key1 <- key_fig_function_LoR(df_species = capelin_Bsim_GrpImpact_EwE_2020_80_20_3LNO,
                                                           df_species_catch= capelin_Bsim_catch_EwE_2020_80_20_3LNO,
                                                           obs_lod_data= 4,
                                                           x_axis_species= "Capelin",
                                                           unique_title ="Capelin LoR - EwE 2020 - 3LNO - Sensitivity",
                                                           legend = "none")

capelin_2020_80_20_3LNO_50_50_key2 <- key_fig_function_LoR_web(LoR_order = c(1, 10, 20, "obs", 30, 40, 50, 60, 70, 80, 90, 100), 
                                                               df_species = capelin_web_Bsim_GrpImpact_EwE_2020_80_20_3LNO,
                                                               obs_LoR_data = 4,
                                                               x_axis_species = "Capelin",
                                                               unique_title ="Capelin LoR - EwE 2020 - 3LNO - Sensitivity",
                                                               legend = "none")

# capelin merged

capelin_2020_80_20_2J3K_3LNO_50_50_key1 <- key_fig_function_LoR_standardized(df_species = capelin_Bsim_GrpImpact_EwE_2020_80_20_merged_2J3K_3LNO,
                                                                             df_species_catch= capelin_Bsim_catch_EwE_2020_80_20_merged_2J3K_3LNO,
                                                                             x_axis_species = "Capelin",
                                                                             unique_title ="Capelin LoR - EwE 2020 - 2J3K-3LNO - Sensitivity",
                                                                             legend = "none")

capelin_2020_80_20_2J3K_3LNO_50_50_key2 <- key_fig_function_LoR_web_standardized(df_species = capelin_web_Bsim_GrpImpact_EwE_2020_80_20_merged_2J3K_3LNO,
                                                                                 x_axis_species = "Capelin",
                                                                                 unique_title ="Capeling LoR - EwE 2020 - 2J3K-3LNO - Sensitivity",
                                                                                 legend = "none")

  ### grid arrange ###
  
  if (species == "cod") {
    
    grid.arrange(cod_2020_2J3K_50_50_key1, cod_2020_2J3K_50_50_key2, 
                 cod_2020_3LNO_50_50_key1, cod_2020_3LNO_50_50_key2,
                 cod_2020_2J3K_3LNO_50_50_key1, cod_2020_2J3K_3LNO_50_50_key2,
                 ncol = 2)
    
  } else if (species == "capelin") {
    
    grid.arrange(capelin_2020_2J3K_50_50_key1, capelin_2020_2J3K_50_50_key2, 
                 capelin_2020_3LNO_50_50_key1, capelin_2020_3LNO_50_50_key2,
                 capelin_2020_2J3K_3LNO_50_50_key1, capelin_2020_2J3K_3LNO_50_50_key2,
                 ncol = 2)
  } else if (species == "cod_sensitivity") {
    
    grid.arrange(cod_2020_80_20_2J3K_50_50_key1, cod_2020_80_20_2J3K_50_50_key2, 
                 cod_2020_80_20_3LNO_50_50_key1, cod_2020_80_20_3LNO_50_50_key2,
                 cod_2020_80_20_2J3K_3LNO_50_50_key1, cod_2020_80_20_2J3K_3LNO_50_50_key2,
                 ncol = 2)
    
  } else if (species == "capelin_sensitivity") {
    
    grid.arrange(capelin_2020_80_20_2J3K_50_50_key1, capelin_2020_80_20_2J3K_50_50_key2, 
                 capelin_2020_80_20_3LNO_50_50_key1, capelin_2020_80_20_3LNO_50_50_key2,
                 capelin_2020_80_20_2J3K_3LNO_50_50_key1, capelin_2020_80_20_2J3K_3LNO_50_50_key2,
                 ncol = 2)
  }
  
}
