
### Heatmap multispecific biomass lod funcion ###

heatmap_multispecific_biomass_LoD_function_seal_top <- function(data, species_number, species_one_fixe, species_two_fixe, species_one, species_two) {
  
  data_process <- data %>% 
    filter(Group_number %in% species_number) %>%
    select(-c(Group_number, Group_name)) %>%
    mutate(across(everything(), as.numeric)) %>%
    summarise_all(.funs = sum) %>%
    pivot_longer(cols = everything(), 
                 names_to = c(paste0("LoD_", species_one_fixe), paste0("LoD_", species_two_fixe)),
                 names_pattern = paste0("LoD_", species_one_fixe, "_(.*?)_LoD_", species_two_fixe, "_(.*)"),
                 values_to = paste0("biomass_", species_two)) %>%
    mutate(across(starts_with("biomass_"), as.numeric)) %>%
    mutate(
      !!paste0("LoD_", species_two) := factor(get(paste0("LoD_", species_two)), levels = c("0", "25", "obs", "50", "75", "99")),
      !!paste0("LoD_", species_one) := factor(get(paste0("LoD_", species_one)), levels = c("99", "75", "50", "25", "obs", "0"))) %>%
    mutate(!!paste0(species_two, "_depletion") := if_else(get(paste0("LoD_", species_one)) == "obs" & get(paste0("LoD_", species_two)) == "0", 
                                                          0, 
                                                          round((pull(., !!paste0("biomass_", species_two))[get(paste0("LoD_", species_one)) == "obs" & get(paste0("LoD_", species_two)) == "0"] - pull(., !!paste0("biomass_", species_two))) / pull(., !!paste0("biomass_", species_two))[get(paste0("LoD_", species_one)) == "obs" & get(paste0("LoD_", species_two)) == "0"], 5) * 100))
  
  ggplot_biomass <- ggplot(data_process, aes_string(x = paste0("LoD_", species_two), y = paste0("LoD_", species_one), fill = paste0("biomass_", species_two))) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colours = c("#d9ed92", "#b5e48c", "#99d98c", "#76c893", "#52b69a", "#34a0a4"), 
                         limits = c(min(data_process[[paste0("biomass_", species_two)]]), max(data_process[[paste0("biomass_", species_two)]])),
                         breaks = pretty_breaks(n = 6)) +
    geom_text(aes(label = round(data_process[[paste0("biomass_", species_two)]], 4)), color = "black", size = 4) +  
    theme_minimal() + 
    scale_x_discrete(position = "top") + 
    labs(title = paste("Heatmap", species_two, "biomass ~ LoD", species_two, "x LoD", species_one),
         x = paste("Level of Depletion", species_two),
         y = paste("Level of Depletion", species_one),
         fill = paste(species_two, "\nbiomass")) +  
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12)) 
  
  suppressWarnings(print(ggplot_biomass))
  
  ggplot_depletion <- ggplot(data_process, aes_string(x = paste0("LoD_", species_two), y = paste0("LoD_", species_one), fill = paste0(species_two, "_depletion"))) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colours = c("#d9ed92", "#b5e48c", "#99d98c", "#76c893", "#52b69a", "#34a0a4"), 
                         limits = c(min(data_process[[paste0(species_two, "_depletion")]]), max(data_process[[paste0(species_two, "_depletion")]])),
                         breaks = pretty_breaks(n = 6)) +
    geom_text(aes(label = round(data_process[[paste0(species_two, "_depletion")]], 4)), color = "black", size = 4) +  
    theme_minimal() + 
    scale_x_discrete(position = "top") + 
    labs(title = paste("Heatmap", species_two, "depletion ~ LoD", species_two, "x LoD", species_one),
         x = paste("Level of Depletion", species_two),
         y = paste("Level of Depletion", species_one),
         fill = paste("LoD", species_two)) +  
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12)) 
  
  suppressWarnings(print(ggplot_depletion))
  
}