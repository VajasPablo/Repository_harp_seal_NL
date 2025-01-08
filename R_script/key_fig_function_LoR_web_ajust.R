
key_fig_function_LoR_web_ajust <- function(LoR_order, df_species, obs_LoR_data, x_axis_species, unique_title, legend) {
  
  keyplot <- ggplot(df_species, aes(x=LoR)) +
    geom_ribbon(aes(x = as.numeric(LoR), ymin = value20_neg, ymax = value20_pos, fill = "Upper 20%"), alpha = 0.8, color = NA) +
    geom_ribbon(aes(x = as.numeric(LoR), ymin = value40_neg, ymax = value40_pos, fill = "Upper 40%"), alpha = 0.8, color = NA) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
    geom_segment(data = df_species, aes(x = obs_LoR_data, xend = obs_LoR_data, y = 0.2, yend = 0),
                 arrow = arrow(length = unit(0.3, "cm")), color = "black", size = 0.5) +
    guides(fill = guide_legend(title = "Impact level")) +
    scale_y_continuous(name = "Prop. impacted group", breaks = seq(-0.4, 0.4, 0.1),
                       labels = function(x) sprintf("%.1f", x),
                       expand = c(0, 0), limits = c(-0.4, 0.4)) +
    scale_x_continuous(breaks = seq(1, 12, 1),
                       labels = LoR_order,
                       expand = expansion(add = c(0, 0))) +
    coord_cartesian(xlim = c(1, 12)) +
    labs(x = paste("LoR", x_axis_species)) +
    scale_fill_manual(values = c("lightblue", "tomato"),
                      labels = c("20%", "40%")) +
    theme_bw() +
    theme(axis.title.x = element_blank(),  
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 12),
          legend.text = element_text(size = 10)) + 
    ggtitle(unique_title) + theme(legend.position = legend)
  
}
