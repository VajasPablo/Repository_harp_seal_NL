
### All key figures ###

fig_assemblage_function_MSE <- function() {
  
  ### 2J3K 50/50 ###
  
  MSE_2J3K_50_50 <- Harp_seal_MSE_simulation_function_2J3K(prop_sensitivity = 0.5, 
                                                           region = "2J3K", 
                                                           data_simulation = Bcod_Bcapelin_simulation_biomass_EwE_2020_2J3K,
                                                           title = "EwE 2020 2J3K \nLoR Cod & Capelin - Harp seal trends")
  ### 3LNO 50/50 ###
  
  MSE_3LNO_50_50 <-  Harp_seal_MSE_simulation_function_3LNO(prop_sensitivity = 0.5, 
                                                            region = "3LNO", 
                                                            data_simulation = Bcod_Bcapelin_simulation_biomass_EwE_2020_3LNO,
                                                            title = "EwE 2020 3LNO \nLoR Cod & Capelin - Harp seal trends")
  ### 2J3K 80/20 ###
  
  MSE_2J3K_80_20 <- Harp_seal_MSE_simulation_function_2J3K_sensitivity(prop_sensitivity = 0.8, 
                                                           region = "2J3K", 
                                                           data_simulation = Bcod_Bcapelin_simulation_biomass_EwE_2020_2J3K_80_20,
                                                           title = "EwE 2020 2J3K Sensitivity \nLoR Cod & Capelin - Harp seal trends")
  ### 3LNO 80/20 ###
  
  MSE_3LNO_80_20 <-  Harp_seal_MSE_simulation_function_3LNO_sensitivity(prop_sensitivity = 0.2, 
                                                            region = "3LNO", 
                                                            data_simulation = Bcod_Bcapelin_simulation_biomass_EwE_2020_3LNO_80_20,
                                                            title = "EwE 2020 3LNO Sensitivity \nLoR Cod & Capelin - Harp seal trends")
  
  ### grid arrange ###
  
  suppressWarnings(grid.arrange(
    MSE_2J3K_50_50,
    MSE_3LNO_50_50,
    MSE_2J3K_80_20,
    MSE_3LNO_80_20,
    ncol = 2))
  
}

