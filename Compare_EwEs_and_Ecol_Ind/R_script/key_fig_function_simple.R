
### Key figure function ###

key_fig_function <- function(df_species, df_species_catch, obs_lod_data, x_axis_species, plot_name, unique_title, legend) {
 
  keyplot <- ggplot(data = df_species, aes(x = LoD, y = Value, color = Variable)) +
    geom_line(size = 1) +
    geom_point(size = 0) + 
    geom_smooth(data = df_species_catch, aes(x = LoD, y = Catch_rate),
                method = "gam",   
                se = FALSE, 
                color = "yellow3") +
    scale_color_manual(values = c("lightblue4", "tomato"),
                       breaks = c("value20", "value40"),
                       labels = c("Upper 20%", "Upper 40%")) +
    geom_segment(aes(x = obs_lod_data, xend = obs_lod_data, y = 0.30, yend = 0),
                 arrow = arrow(length = unit(0.5, "cm")), color = "black", size = 1) +
    guides(color = guide_legend(title = "Prop. Group Impacted")) +
    scale_y_continuous(name = "Prop. impacted group", breaks = seq(0, 1, 0.25),   
                       labels = seq(0, 1, 0.25), expand = c(0, 0),
                       sec.axis = sec_axis(~ ., name = "MSY",
                                           breaks = seq(0, 1, 0.25),   
                                           labels = seq(0, 1, 0.25))) +
    labs(x = paste("LoD", x_axis_species)) +
    theme_bw() +
    coord_cartesian(ylim = c(0, 1.05)) +
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12)) + 
    ggtitle(unique_title) + theme(legend.position = legend)
   
  assign(plot_name, keyplot, envir = .GlobalEnv)  
  
}

