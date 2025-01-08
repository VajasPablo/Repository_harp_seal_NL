
### Heatmap multispecific biomass lod function ###

heatmap_multispecific_biomass_growth_function <- function(data, species_number, species_one_fixe, species_two_fixe, species_one, species_two, target_species, sp2_lvl, sp1_lvl, period, obs_dens) {
  
  data_process <- data %>%
    filter(Group_number %in% species_number) %>%
    select(-c(Group_number, Group_name)) %>%
    mutate(across(everything(), as.numeric)) %>%
    summarise_all(.funs = sum) %>%
    pivot_longer(cols = everything(), 
                 names_to = c(paste0("LoD_", species_one_fixe), paste0("LoD_", species_two_fixe)),
                 names_pattern = paste0("LoD_", species_one_fixe, "_(.*?)_LoD_", species_two_fixe, "_(.*)"),
                 values_to = paste0("biomass_", target_species)) %>%
    mutate(across(starts_with("biomass_"), as.numeric)) %>%
    mutate(
      !!paste0("LoD_", species_two) := factor(get(paste0("LoD_", species_two)), levels = sp2_lvl),
      !!paste0("LoD_", species_one) := factor(get(paste0("LoD_", species_one)), levels = sp1_lvl)) %>%
    mutate(!!paste0("biomass_", target_species) := if_else(get(paste0("LoD_", species_one)) == "obs" & get(paste0("LoD_", species_two)) == "obs", 
                                                           0, 
                                                           (get(paste0("biomass_", target_species)) - get(paste0("biomass_", target_species))[get(paste0("LoD_", species_one)) == "obs" & get(paste0("LoD_", species_two)) == "obs"]) / get(paste0("biomass_", target_species))[get(paste0("LoD_", species_one)) == "obs" & get(paste0("LoD_", species_two)) == "obs"] * 100)
    ) %>%
    mutate(!!paste0(target_species, "_depletion") := if_else(get(paste0("LoD_", species_one)) == "obs" & get(paste0("LoD_", species_two)) == "0", 
                                                             0, 
                                                             round((pull(., !!paste0("biomass_", target_species))[get(paste0("LoD_", species_one)) == "obs" & get(paste0("LoD_", species_two)) == "0"] - pull(., !!paste0("biomass_", target_species))) / pull(., !!paste0("biomass_", target_species))[get(paste0("LoD_", species_one)) == "obs" & get(paste0("LoD_", species_two)) == "0"], 5) * 100)
    ) %>%
    mutate(is_positive = if_else(get(paste0("biomass_", target_species)) > 0, "positive", "non-positive"))
  
  ggplot_biomass <- ggplot(data_process, aes_string(x = paste0("LoD_", species_two), y = paste0("LoD_", species_one), fill = paste0("biomass_", target_species))) +
    geom_tile(aes_string(color = "is_positive"), size = 1.5) +
    scale_color_manual(values = c("positive" = "red", "non-positive" = "transparent"), guide = "none") +
    scale_fill_gradientn(colours = c("#d9ed92", "#b5e48c", "#99d98c", "#76c893", "#52b69a", "#34a0a4"), 
                         limits = c(min(data_process[[paste0("biomass_", target_species)]]), max(data_process[[paste0("biomass_", target_species)]])),
                         breaks = pretty_breaks(n = 6)) +
    geom_text(aes(label = paste0(round(data_process[[paste0("biomass_", target_species)]], 2), "%")), color = "black", size = 4) +  
    theme_minimal() + 
    scale_x_discrete(position = "top") + 
    labs(title = paste(target_species, "growth ~ LoD", species_two, "x LoD", species_one, "\n", period, "-", obs_dens, "t/km2"),
         x = paste("Level of Depletion", species_two),
         y = paste("Level of Depletion", species_one),
         fill = ifelse(target_species == "seal", "Harp seal\ngrowth (%)", paste(target_species, "\ngrowth (%)"))) +  
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12)) 
  
}
