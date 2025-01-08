

fig_assemblage_function_LoR <- function() {
  
  ### 2J3K 50/50 ###
  
 heat_2020_2J3K_50_50 <- heatmap_multispecific_biomass_LoR_function_2J3K(data = Bcod_Bcapelin_simulation_biomass_EwE_2020_2J3K,
                                                                         spe_title = "EwE 2020 2J3K - \nLoR Cod & Capelin - Harp seal trends")
  
  ### 3LNO 50/50 ###
  
 heat_2020_3LNO_50_50 <- heatmap_multispecific_biomass_LoR_function_3LNO(data = Bcod_Bcapelin_simulation_biomass_EwE_2020_3LNO,
                                                                          spe_title = "EwE 2020 3LNO - \nLoR Cod & Capelin - Harp seal trends")
  
 ### 2J3K 80/20 ###
 
 heat_2020_2J3K_80_20 <- heatmap_multispecific_biomass_LoR_function_2J3K(data = Bcod_Bcapelin_simulation_biomass_EwE_2020_2J3K_80_20,
                                                                         spe_title = "EwE 2020 2J3K sensitivity - \nLoR Cod & Capelin - Harp seal trends")
 
 ### 3LNO 80/20 ###
 
 heat_2020_3LNO_80_20 <- heatmap_multispecific_biomass_LoR_function_3LNO(data = Bcod_Bcapelin_simulation_biomass_EwE_2020_3LNO_80_20,
                                                                          spe_title = "EwE 2020 3LNO sensitivity - \nLoR Cod & Capelin - Harp seal trends")
 
  
  ### grid arrange ###
  
  suppressWarnings(grid.arrange(
    heat_2020_2J3K_50_50,
    heat_2020_3LNO_50_50,
    heat_2020_2J3K_80_20,
    heat_2020_3LNO_80_20,
    ncol = 2))
  
}
  