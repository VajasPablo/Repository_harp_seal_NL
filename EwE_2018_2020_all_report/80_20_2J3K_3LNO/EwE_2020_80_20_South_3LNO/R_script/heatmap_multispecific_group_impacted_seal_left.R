
### Heatmap for multispecific simulations of groups impacted ###

heatmap_multispecific_group_impacted_seal_left <- function(data, species_one_fixe, species_two_fixe, species_one, species_two, percent) {
  
  data_process <- data %>% 
    pivot_longer(cols = starts_with("LoD"), 
                 names_to = c(paste0("LoD_", species_one_fixe), paste0("LoD_", species_two_fixe)),
                 names_pattern = paste0("LoD_", species_one_fixe, "_(.*?)_LoD_", species_two_fixe, "_(.*)"),
                 values_to =  "value") %>% 
    pivot_wider(names_from = Description, values_from = value) %>%
    mutate(
      !!paste0("LoD_", species_two) := factor(get(paste0("LoD_", species_two)), levels = c("0", "obs", "25", "50", "75", "99")),
      !!paste0("LoD_", species_one) := factor(get(paste0("LoD_", species_one)), levels = c("99", "75", "50","obs", "25", "0"))) 
  
  ggplot_list <- list()  # Créer une liste pour stocker les graphiques
  
  for (i in 3:8) {  # Boucle sur les colonnes 3 à 8
    ggplot_list[[i-2]] <- ggplot(data_process, aes_string(x = paste0("LoD_", species_two), y = paste0("LoD_", species_one), fill = paste0("`", names(data_process)[i], "`"))) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colours = c("#d9ed92", "#b5e48c", "#99d98c", "#76c893", "#52b69a", "#34a0a4"), 
                           limits = c(min(data_process[[i]], na.rm = TRUE), max(data_process[[i]], na.rm = TRUE)),
                           breaks = pretty_breaks(n = 5)) +
      geom_text(aes_string(label = paste0("`", names(data_process)[i], "`")), color = "black", size = 4) +  
      theme_minimal() + 
      scale_x_discrete(position = "top") + 
      labs(x = paste("Level of Depletion", species_two),
           y = paste("Level of Depletion", species_one),
           fill = paste0(names(data_process)[i], "\ngroups", "\nimpacted", "\nupper ", percent)) +  
      theme(axis.title.x = element_text(size = 10), 
            axis.title.y = element_text(size = 10), 
            axis.text.x = element_text(size = 10),  
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 12),  
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 10)) 
  }
  
  grid.arrange(grobs = ggplot_list, ncol = 2)  # Imprimer les graphiques avec grid.arrange
  
}
