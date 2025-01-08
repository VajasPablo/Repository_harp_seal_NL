
### heatmap function generate relative biomass plot ###

heatmap_monospe_relative_biomass <- function(data, species_name){
  
  column_names <- paste0("LoD ", c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99))
  
  df_normalized <- data %>%
    rename_with(~ gsub("biomass_annual_", "", .), starts_with("biomass_annual_")) %>%
    rename_with(~ column_names, starts_with("F")) %>%
    select(-1) %>%
    mutate(across(-Group_name, as.numeric)) %>%
    pivot_longer(cols = -Group_name,
                 names_to = "Simulation",
                 values_to = "Value") %>%
    mutate(Value = as.numeric(Value)) %>%
    mutate(Group_name = factor(Group_name, levels = rev(unique(Group_name)))) %>%
    group_by(Group_name) %>%
    mutate(Value_normalized = (Value - min(Value)) / (max(Value) - min(Value))) %>%
    ungroup()
  
  heatmap_biomass_relative <- ggplot(df_normalized, aes(x = Simulation, y = Group_name, fill = Value_normalized)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal() +
    labs(x = paste0(species_name, " LoD simulations"), y = "Ecopath species groups", fill = "Relative \nBiomass") +
    theme_minimal() + 
    scale_x_discrete(position = "top") + 
    theme(axis.text.x = element_text(angle = 20, hjust = 0)) +
    ggtitle(paste0("Relative change in biomass per ", species_name, " LoD")) +
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12)) 
  
  print(heatmap_biomass_relative)
  
}
