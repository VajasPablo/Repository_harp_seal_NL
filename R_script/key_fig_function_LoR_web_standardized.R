
key_fig_function_LoR_web_standardized <- function(df_species, x_axis_species, unique_title, legend) {
  
  LoR_order <- c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
  
  keyplot <- ggplot(df_species, aes(x=LoR)) +
    geom_ribbon(aes(x = as.numeric(LoR), ymin = value20_neg, ymax = value20_pos, fill = "Upper 20%"), alpha = 0.8, color = NA) +
    geom_ribbon(aes(x = as.numeric(LoR), ymin = value40_neg, ymax = value40_pos, fill = "Upper 40%"), alpha = 0.8, color = NA) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
    guides(fill = guide_legend(title = "Prop. Group Impacted")) +
    scale_y_continuous(name = "Prop. impacted group", breaks = seq(-0.5, 0.5, 0.1),
                       labels = seq(-0.5, 0.5, 0.1), expand = c(0, 0), limits = c(-0.5, 0.5)) +
    scale_x_continuous(breaks = seq(1, 11, 1),
                       labels = LoR_order,
                       expand = expansion(add = c(0, 0))) +
    coord_cartesian(xlim = c(1, 11)) +
    labs(x = paste("LoR", x_axis_species)) +
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

}
