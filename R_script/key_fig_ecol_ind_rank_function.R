
# Generic plot function Ecological indicator Key figure

key_fig_ecol_ind_rank_function <- function(ecol_ind_data, xaxis, yaxis, xlab, ylab) {
  
  data_clean <- ecol_ind_data %>% filter(!Species %in% "Cod all")
  
  xaxis_sym <- sym(xaxis)
  yaxis_sym <- sym(yaxis)
  
 gplot <- ggplot(data_clean, aes(x = !!xaxis_sym, y = !!yaxis_sym, color = `Periods-Areas`, shape = Species)) +
    geom_point(size = 5) +
    scale_color_manual(values = c("1985-1987" = "#ef476f", "2013-2015" = "#f4a261", "2018-2020 NL Shelf" = "#06d6a0", "2018-2020 Grand Banks" = "#0077b6")) +
    scale_shape_manual(values = c("Harp seal" = 10, "Cod > 35cm" = 16, "Cod ≤ 35cm" = 17, "Capelin" = 8)) +
    labs(x = xlab,
         y = ylab,
         color = "Periods-Areas",
         shape = "Species") +     
    theme_bw() +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 13))

 return(gplot)
  
}
