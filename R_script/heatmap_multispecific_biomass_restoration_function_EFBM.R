
heatmap_multispecific_biomass_restoration_function_EFBM <- function(data, species_number, species_one_fixe, species_two_fixe, species_one, species_two, target_species, period, obs_dens, hist_val) {
  
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
    mutate(!!paste0("LoD_", species_two) := factor(get(paste0("LoD_", species_two)), levels = c("0", "obs", "25", "50", "75", "99")), 
           !!paste0("LoD_", species_one) := factor(get(paste0("LoD_", species_one)), levels = c("99", "75", "50", "25", "obs", "0"))) %>% 
    mutate(!!paste0("biomass_", target_species) := ((get(paste0("biomass_", target_species)) - hist_val) / hist_val) * 100) %>% 
    mutate(!!paste0("biomass_", target_species) := 100 - abs(get(paste0("biomass_", target_species)))) %>% 
    mutate(is_positive = if_else(get(paste0("biomass_", target_species)) > 84, "Healthy", 
                                 if_else(get(paste0("biomass_", target_species)) > 25, "Cautious",  
                                         "Critical"))) # Classifying zones based on new boundaries 
  
  ggplot_biomass <- ggplot(data_process, aes_string(x = paste0("LoD_", species_two), y = paste0("LoD_", species_one), fill = "is_positive")) + 
    geom_tile(size = 1.5) + 
    scale_fill_manual(
      values = c(
        "Healthy" = scales::alpha("#008000", 0.75),  # Green with 80% opacity
        "Cautious" = scales::alpha("#FFA500", 0.75),  # Orange with 80% opacity
        "Critical" = scales::alpha("#FF6347", 0.75)  # Tomato with 80% opacity
      ),
      name = "Status",
      limits = c("Healthy", "Cautious", "Critical")
    ) +
    geom_text(aes(label = paste0(round(data_process[[paste0("biomass_", target_species)]], 0), "%")), color = "black", size = 4) + 
    theme_minimal() + 
    scale_x_discrete(position = "top") + 
    labs( 
      x = paste(ifelse(species_two == "seal", "Harp seal", 
                       ifelse(species_two == "cod", "Cod", 
                              ifelse(species_two == "capelin", "Capelin", species_two))), 
                "LoD (%)"),
      y = paste(ifelse(species_one == "seal", "Harp seal", 
                       ifelse(species_one == "cod", "Cod", 
                              ifelse(species_one == "capelin", "Capelin", species_one))), 
                "LoD (%)"),
      fill = ifelse(target_species == "seal", "Harp seal\nrestoration (%)", paste0("Percentage of", "\n", target_species, " Restoration", "\n", "based on EwE 1985", "\n")) 
    ) + 
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          plot.title = element_text(size = 16),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12)) 
  
  return(ggplot_biomass)
}