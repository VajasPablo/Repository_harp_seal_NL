
key_fig_function_LoD_web <- function(df_species, obs_lod_data, x_axis_species, plot_name, unique_title, legend) {
  
  LoD_order = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99)
  
  obs_LoD_data_position <- approx(LoD_order, seq_along(LoD_order), obs_lod_data)$y
  
  keyplot <- ggplot(df_species, aes(x=LoD)) +
    geom_ribbon(aes(x = as.numeric(LoD), ymin = value20_neg, ymax = value20_pos, fill = "Upper 20%"), alpha = 0.8, color = NA) +
    geom_ribbon(aes(x = as.numeric(LoD), ymin = value40_neg, ymax = value40_pos, fill = "Upper 40%"), alpha = 0.8, color = NA) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
    geom_segment(aes(x = obs_LoD_data_position, xend = obs_LoD_data_position, y = 0.30, yend = 0),
                 arrow = arrow(length = unit(0.5, "cm")), color = "black", size = 1) +
    guides(fill = guide_legend(title = "Prop. Group Impacted")) +
    scale_y_continuous(name = "Prop. impacted group", breaks = seq(-0.5, 0.5, 0.1),
                       labels = seq(-0.5, 0.5, 0.1), expand = c(0, 0), limits = c(-0.5, 0.5)) +
    scale_x_continuous(breaks = seq(1, 11, 1),
                       labels = LoD_order,
                       expand = expansion(add = c(0, 0))) +
    coord_cartesian(xlim = c(1, 11)) +
    labs(x = paste("LoD", x_axis_species)) +
    scale_fill_manual(values = c("lightblue", "tomato"),
                      labels = c("Upper 20%", "Upper 40%")) +
    theme_bw() +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12)) + 
    ggtitle(unique_title) + theme(legend.position = legend)
  
  assign(plot_name, keyplot, envir = .GlobalEnv)  
  
}
