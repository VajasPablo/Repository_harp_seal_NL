
# Generic plot function Ecological indicator Key figure

key_fig_ecol_ind_avg_function <- function(ecol_ind_data, xaxis, yaxis, xlab, ylab) {
  
  data_clean <- ecol_ind_data %>% filter(!Species %in% "Cod all")
  
  data_regression <- data.frame(x = data_clean[[xaxis]], y = predict(lm(formula = as.formula(paste(yaxis, "~", xaxis)), data = data_clean), newdata = data_clean))
  
  rsquare <- summary(lm(formula = as.formula(paste(yaxis, "~", xaxis)), data = data_clean))$r.squared
  pvalue <- summary(lm(formula = as.formula(paste(yaxis, "~", xaxis)), data = ecol_ind_data))$coefficients[2, 4]
  r_squared_text <- paste("R² = ", sprintf("%.2f", rsquare), "; P-value =", sprintf("%.2f", pvalue))
  
  gplot <- ggplot(data_clean, aes(x = !!sym(xaxis), y = !!sym(yaxis), color = `Periods-Areas`, shape = Species)) +
    geom_point(size = 5) +
    geom_line(data = data_regression, aes(y = y, x = x), color = "black", inherit.aes = FALSE) +
    annotate("text", 
             x = max(data_clean[[xaxis]], na.rm = TRUE) / 2, 
             y = max(data_clean[[yaxis]], na.rm = TRUE)* 9/10, 
             label = r_squared_text, 
             hjust = 0.5, 
             vjust = -0.5, 
             size = 4, 
             color = "black") +
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






