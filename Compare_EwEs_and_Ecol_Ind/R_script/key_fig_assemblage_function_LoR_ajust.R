
### All key figures ###

key_fig_assemblage_function_LoR_ajust <- function(species){
  

  # cod 2J3K
  
  cod_2020_2J3K_50_50_key2 <- key_fig_function_LoR_web_ajust(LoR_order = c(1, 10, 20, 30, 40, 50, 60,"obs", 70, 80, 90, 100),
                                                       df_species = cod_web_Bsim_GrpImpact_EwE_2020_2J3K,
                                                       obs_LoR_data = 8,
                                                       x_axis_species = "Cod",
                                                       unique_title ="2018-2020 NL Shelf: Cod",
                                                       legend = "none")
  
  # cod 3LNO
  
  
  cod_2020_3LNO_50_50_key2 <- key_fig_function_LoR_web_ajust(LoR_order = c(1, 10, "obs", 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                                                       df_species = cod_web_Bsim_GrpImpact_EwE_2020_3LNO,
                                                       obs_LoR_data = 3,
                                                       x_axis_species = "Cod",
                                                       unique_title ="2018-2020 Grand Banks: Cod",
                                                       legend = "none")
  
  
  # capelin 2J3K
  
  
  capelin_2020_2J3K_50_50_key2 <- key_fig_function_LoR_web_ajust(LoR_order = c(1, 10, 20, "obs", 30, 40, 50, 60, 70, 80, 90, 100),
                                                           df_species = capelin_web_Bsim_GrpImpact_EwE_2020_2J3K,
                                                           obs_LoR_data = 4,
                                                           x_axis_species = "Capelin",
                                                           unique_title ="2018-2020 NL Shelf: Capelin",
                                                           legend = "none")
  
  # capelin 3LNO
  
  
  capelin_2020_3LNO_50_50_key2 <- key_fig_function_LoR_web_ajust(LoR_order = c(1, 10, 20, "obs", 30, 40, 50, 60, 70, 80, 90, 100),
                                                           df_species = capelin_web_Bsim_GrpImpact_EwE_2020_3LNO,
                                                           obs_LoR_data = 4,
                                                           x_axis_species = "Capelin",
                                                           unique_title ="2018-2020 Grand Banks: Capelin",
                                                           legend = "none")
  
  ### grid arrange ###
  
  plots <- list(cod_2020_2J3K_50_50_key2,
                capelin_2020_2J3K_50_50_key2, 
                cod_2020_3LNO_50_50_key2,
                capelin_2020_3LNO_50_50_key2)
  
  # margin 
  
  plots <- lapply(plots, function(p) {
    p + theme(plot.margin = margin(t = 5, r = 20, b = 10, l = 10))
  })
  
  # Utilisez ggarrange pour combiner les graphiques
  combined_plot <- ggarrange(plotlist = plots, ncol = 2, nrow = 2,
                             common.legend = TRUE, legend = "right")
  
  combined_plot <- annotate_figure(combined_plot,
                                   bottom = text_grob("Level of Recovery (%)", face = "bold", size = 14),
                                   left = text_grob("Proportion of Groups Impacted", face = "bold", size = 14, rot = 90))

  print(combined_plot)

}
