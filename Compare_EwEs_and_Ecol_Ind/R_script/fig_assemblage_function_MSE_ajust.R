
### All key figures ###

fig_assemblage_function_MSE_ajust <- function() {
  
  
  theme_custom <-  theme(axis.title.x = element_blank(),  
                         axis.title.y = element_blank(),
                         axis.text.x = element_text(size = 12),
                         axis.text.y = element_text(size = 12),
                         plot.title = element_text(size = 14),
                         legend.text = element_text(size = 12))
  
  ### 2J3K 50/50 ###
  
  MSE_2J3K_50_50 <- Harp_seal_MSE_simulation_function_2J3K_ajust(prop_sensitivity = 0.5, 
                                                           region = "2J3K", 
                                                           data_simulation = Bcod_Bcapelin_simulation_biomass_EwE_2020_2J3K,
                                                           title = "2018-2020 NL Shelf - 50% Pop. Distr.") + theme_custom
  ### 3LNO 50/50 ###
  
  MSE_3LNO_50_50 <-  Harp_seal_MSE_simulation_function_3LNO_ajust(prop_sensitivity = 0.5, 
                                                            region = "3LNO", 
                                                            data_simulation = Bcod_Bcapelin_simulation_biomass_EwE_2020_3LNO,
                                                            title = "2018-2020 Grand Banks - 50% Pop. Distr.") + theme_custom
  ### 2J3K 80/20 ###
  
  MSE_2J3K_80_20 <- Harp_seal_MSE_simulation_function_2J3K_sensitivity_ajust(prop_sensitivity = 0.8, 
                                                                       region = "2J3K", 
                                                                       data_simulation = Bcod_Bcapelin_simulation_biomass_EwE_2020_2J3K_80_20,
                                                                       title = "2018-2020 NL Shelf - 80% Pop. Distr.") + theme_custom
  ### 3LNO 80/20 ###
  
  MSE_3LNO_80_20 <-  Harp_seal_MSE_simulation_function_3LNO_sensitivity_ajust(prop_sensitivity = 0.2, 
                                                                        region = "3LNO", 
                                                                        data_simulation = Bcod_Bcapelin_simulation_biomass_EwE_2020_3LNO_80_20,
                                                                        title = "2018-2020 Grand Banks - 20% Pop. Distr.") + theme_custom
  
  ### grid arrange ###
  
  plots <- list(MSE_2J3K_50_50,
                MSE_3LNO_50_50,
                MSE_2J3K_80_20,
                MSE_3LNO_80_20)
  
  # Utilisez ggarrange pour combiner les graphiques
  combined_plot <- ggarrange(plotlist = plots, ncol = 2, nrow = 2,
                             common.legend = TRUE, legend = "right")
  
  combined_plot <- annotate_figure(combined_plot,
                                   bottom = text_grob("Year", face = "bold", size = 14),
                                   left = text_grob("Harp seal Relative Biomass Change", face = "bold", size = 14, rot = 90))
  
  print(combined_plot)

}

