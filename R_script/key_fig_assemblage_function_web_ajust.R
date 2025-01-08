
### All key figures ###

key_fig_assemblage_function_web_ajust <- function(){
  
  ### 1985 ###
  
  key_fig_function_LoD_web_ajust(df_species = seal_web_GrpImpact_EwE_1985,
                           obs_lod_data = observed_LoD_Fseal_1985,
                           x_axis_species = "Harp seal",
                           plot_name = "Plot_seal_1985_web",
                           unique_title= "1985-1987: Harp seal", 
                           legend = "none")
  
  key_fig_function_LoD_web_ajust(df_species = cod_web_GrpImpact_EwE_1985,
                           obs_lod_data = observed_LoD_Fcod_1985,
                           x_axis_species = "Cod",
                           plot_name = "Plot_cod_1985_web",
                           unique_title= "1985-1987: Cod", 
                           legend = "none")
  
  key_fig_function_LoD_web_ajust(df_species = capelin_web_GrpImpact_EwE_1985,
                           obs_lod_data = observed_LoD_Fcapelin_1985,
                           x_axis_species = "Capelin",
                           plot_name = "Plot_capelin_1985_web",
                           unique_title= "1985-1987: Capelin", 
                           legend = "none")
  
  ### 2013 ###
  
  key_fig_function_LoD_web_ajust(df_species = seal_web_GrpImpact_EwE_2013,
                           obs_lod_data = observed_LoD_Fseal_2013,
                           x_axis_species = "Harp seal",
                           plot_name = "Plot_seal_2013_web",
                           unique_title= "2013-2015: Harp seal", 
                           legend = "none")
  
  key_fig_function_LoD_web_ajust(df_species = cod_web_GrpImpact_EwE_2013,
                           obs_lod_data = observed_LoD_Fcod_2013,
                           x_axis_species = "Cod",
                           plot_name = "Plot_cod_2013_web",
                           unique_title= "2013-2015: Cod", 
                           legend = "none")
  
  key_fig_function_LoD_web_ajust(df_species = capelin_web_GrpImpact_EwE_2013,
                           obs_lod_data = observed_LoD_Fcapelin_2013,
                           x_axis_species = "Capelin",
                           unique_title= "2013-2015: Capelin",
                           plot_name = "Plot_capelin_2013_web",
                           legend = "none")
  
  ### 2020 2J3K ###
  
  key_fig_function_LoD_web_ajust(df_species = seal_web_GrpImpact_EwE_2020_2J3K,
                           obs_lod_data = observed_LoD_Fseal_2020_2J3K,
                           x_axis_species = "Harp seal",
                           plot_name = "Plot_seal_2020_2J3K_web",
                           unique_title= "2018-2020 NL Shelf: Harp seal", 
                           legend = "none")
  
  key_fig_function_LoD_web_ajust(df_species = cod_web_GrpImpact_EwE_2020_2J3K,
                           obs_lod_data = observed_LoD_Fcod_2020_2J3K,
                           x_axis_species = "Cod",
                           plot_name = "Plot_cod_2020_2J3K_web",
                           unique_title= "2018-2020 NL Shelf: Cod", 
                           legend = "none")
  
  key_fig_function_LoD_web_ajust(df_species = capelin_web_GrpImpact_EwE_2020_2J3K,
                           obs_lod_data = observed_LoD_Fcapelin_2020_2J3K,
                           x_axis_species = "Capelin",
                           unique_title= "2018-2020 NL Shelf: Capelin",
                           plot_name = "Plot_capelin_2020_2J3K_web",
                           legend = "none")
  
  ### 2020 3LNO ###
  
  key_fig_function_LoD_web_ajust(df_species = seal_web_GrpImpact_EwE_2020_3LNO,
                           obs_lod_data = observed_LoD_Fseal_2020_3LNO,
                           x_axis_species = "Harp seal",
                           plot_name = "Plot_seal_2020_3LNO_web",
                           unique_title= "2018-2020 Grand Banks: Harp seal", 
                           legend = "none")
  
  key_fig_function_LoD_web_ajust(df_species = cod_web_GrpImpact_EwE_2020_3LNO,
                           obs_lod_data = observed_LoD_Fcod_2020_3LNO,
                           x_axis_species = "Cod",
                           plot_name = "Plot_cod_2020_3LNO_web",
                           unique_title= "2018-2020 Grand Banks: Cod", 
                           legend = "none")
  
  key_fig_function_LoD_web_ajust(df_species = capelin_web_GrpImpact_EwE_2020_3LNO,
                           obs_lod_data = observed_LoD_Fcapelin_2020_3LNO,
                           x_axis_species = "Capelin",
                           unique_title= "2018-2020 Grand Banks: Capelin",
                           plot_name = "Plot_capelin_2020_3LNO_web",
                           legend = "none")
  
  
  plots <- list(Plot_seal_1985_web, Plot_cod_1985_web, Plot_capelin_1985_web,
                Plot_seal_2013_web, Plot_cod_2013_web, Plot_capelin_2013_web,
                Plot_seal_2020_2J3K_web, Plot_cod_2020_2J3K_web, Plot_capelin_2020_2J3K_web,
                Plot_seal_2020_3LNO_web, Plot_cod_2020_3LNO_web, Plot_capelin_2020_3LNO_web)
  
  # Utilisez ggarrange pour combiner les graphiques
  combined_plot <- ggarrange(plotlist = plots, ncol = 3, nrow = 4,
                             common.legend = TRUE, legend = "right")
  
  combined_plot <- annotate_figure(combined_plot,
                                   bottom = text_grob("Level of Depletion (%)", face = "bold", size = 14),
                                   left = text_grob("Proportion of Groups Impacted", face = "bold", size = 14, rot = 90))
  
  print(combined_plot)

}


