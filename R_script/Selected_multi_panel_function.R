
Selected_multi_panel_function <- function(short_or_long) {
  
  Species_scale_summary_table_large <- Species_scale_summary_table_large %>%
    mutate(`Periods-Areas` = case_when(
      `Periods-Areas` == "2018-2020 2J3K" ~ "2018-2020 NL Shelf",
      `Periods-Areas` == "2018-2020 3LNO" ~ "2018-2020 Grand Banks",
      TRUE ~ `Periods-Areas` 
    ))
  
  key2_rti <- key_fig_ecol_ind_rank_function(ecol_ind_data = Species_scale_summary_table_large, 
                                             xaxis = "RTI", 
                                             yaxis = "Key_2", 
                                             xlab = "Relative Total Impact", 
                                             ylab = "Keystoneness #2") + ggtitle("a)")
  
  rank_ra <- key_fig_ecol_ind_rank_function(ecol_ind_data = Species_scale_summary_table_large, 
                                            xaxis = "Relative_Biomass", 
                                            yaxis = "Rank", 
                                            xlab = "Relative Biomass", 
                                            ylab = "Rank of largest effect") + ggtitle("b)")
  
  rank_key2 <- key_fig_ecol_ind_rank_function(ecol_ind_data = Species_scale_summary_table_large, 
                                              xaxis = "Key_2", 
                                              yaxis = "Rank", 
                                              xlab = "Keystoneness #2", 
                                              ylab = "Rank of largest effect") + ggtitle("c)")
  
  rank_surf <- key_fig_ecol_ind_rank_function(ecol_ind_data = Species_scale_summary_table_large, 
                                              xaxis = "SURF", 
                                              yaxis = "Rank", 
                                              xlab = "SURF index", 
                                              ylab = "Rank of largest effect") + ggtitle("d)")
  
  avg_rb <- key_fig_ecol_ind_avg_function(ecol_ind_data = Species_scale_summary_table_large, 
                                          xaxis = "Relative_Biomass", 
                                          yaxis = "Avg_impact", 
                                          xlab = "Relative Biomass", 
                                          ylab = "Avg. ecosystem impact") + ggtitle("e)")
  
  avg_surf <- key_fig_ecol_ind_avg_function(ecol_ind_data = Species_scale_summary_table_large, 
                                            xaxis = "SURF", 
                                            yaxis = "Avg_impact", 
                                            xlab = "SURF index", 
                                            ylab = "Avg. ecosystem impact") + ggtitle("f)")
  
  combined_plot_short <- (key2_rti + rank_ra +
                            rank_key2 + rank_surf) + 
    plot_layout(ncol = 2, guides = "collect")
  
  combined_plot_long <- (key2_rti + rank_ra +
                           rank_key2 + rank_surf +
                           avg_rb + avg_surf) + 
    plot_layout(ncol = 2, guides = "collect")
  
  # Utilisation de if/else pour retourner le bon graphique
  if (short_or_long == "short") {
    return(combined_plot_short)
  } else if (short_or_long == "long") {
    return(combined_plot_long)
  } else {
    stop("Invalid input: please specify 'short' or 'long'")
  }
  
}