
### Function to create heatmap for explore other species biomass in multispecific simulation ###

create_heatmap_multispe_biomass_LoR <- function(data, SPECIE_NAME, species_one_fixe, species_two_fixe, species_one, species_two) {
  
  data_process <- data %>% 
    filter(Group_name == SPECIE_NAME) %>% 
    select(-c(Group_number, Group_name)) %>% 
    mutate(across(everything(), as.numeric)) %>%
    summarise_all(.funs = sum) %>%
    pivot_longer(cols = everything(), 
                 names_to = c(paste0("LoR_", species_one_fixe), paste0("LoR_", species_two_fixe)),
                 names_pattern = paste0("LoR_", species_one_fixe, "_(.*?)_LoR_", species_two_fixe, "_(.*)"),
                 values_to = "value") %>%
    mutate(
      !!paste0("LoR_", species_two) := factor(get(paste0("LoR_", species_two)), levels = c("1", "obs", "25", "50", "75", "100")),
      !!paste0("LoR_", species_one) := factor(get(paste0("LoR_", species_one)), levels = c("100", "75","obs", "50", "25", "1"))) %>%
    mutate(
      Value_normalized = (value - min(value)) / (max(value) - min(value)))
  
  absolute_heatmap <- ggplot(data_process, aes_string(x = paste0("LoR_", species_two), y = paste0("LoR_", species_one), fill = "value" )) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colours = c("white", "steelblue"), 
                         limits = c(min(data_process$value), max(data_process$value))) +
    geom_text(aes(label = round(value,4)), color = "black", size = 4) +  
    labs(title = paste("Heatmap", SPECIE_NAME, "biomass ~ LoR", species_two, "x LoR", species_one),
         x = paste("Level of Restoration", species_two),
         y = paste("Level of Restoration", species_one),
         fill = paste(SPECIE_NAME, "\nbiomass")) +   
    theme_minimal() + 
    scale_x_discrete(position = "top") + 
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12))
  
  relative_heatmap <- ggplot(data_process, aes_string(x = paste0("LoR_", species_two), y = paste0("LoR_", species_one), fill = "Value_normalized" )) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colours = c("white", "steelblue"), 
                         limits = c(min(data_process$Value_normalized), max(data_process$Value_normalized))) +
    geom_text(aes(label = round(Value_normalized,4)), color = "black", size = 4) +  
    labs(title = paste("Heatmap", SPECIE_NAME, "relative biomass ~ LoR", species_two, "x LoR", species_one),
         x = paste("Level of Restoration", species_two),
         y = paste("Level of Restoration", species_one),
         fill = paste(SPECIE_NAME, "\nrelative biomass")) +   
    theme_minimal() + 
    scale_x_discrete(position = "top") + 
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12))
  
  print(absolute_heatmap)
  
  print(relative_heatmap)
  
}
