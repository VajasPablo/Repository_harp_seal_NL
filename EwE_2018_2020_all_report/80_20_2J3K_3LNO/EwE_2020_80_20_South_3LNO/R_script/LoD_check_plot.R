
### LoD diagnostic plot ###

LoD_check_plot <- function(data, observed_biomass, Fspecies, digit_import_value, species, Group_number_specific) {
  # Filter data and pivot longer
  LoD <- data %>%
    filter(Group_number %in% Group_number_specific) %>%
    select(-c(Group_number, Group_name)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "F_sim", 
                 values_to = "F_sim_value") %>%
    mutate(
      F_sim_value = as.numeric(F_sim_value),  
      F_depletion = paste0(F_sim, "_Depletion"),  
      F_depletion_value = round(abs(((F_sim_value - F_sim_value[1]) / F_sim_value[1]) * 100), 2),
      F_sim = gsub("biomass_annual_", "", F_sim)
    ) 
  
  # Calculate observed biomass
  
  column_name <- paste0("biomass_annual_", Fspecies, "_0.", paste(rep("0", digit_import_value), collapse = ""), sep = "")
  F0 <- data %>%
    filter(Group_number %in% Group_number_specific) %>%
    pull(column_name) %>%
    as.numeric()
  
  observed_LoD <- ((F0 - observed_biomass) / F0) * 100
  
  # Plot
  ggplotcheck <- ggplot(LoD, aes(x = F_depletion_value, y = F_sim)) +
    geom_point() +
    labs(x = "Level of Depletion", y = "F rate", title = paste0(species, ": Plot of % depletion vs. F rate")) +
    theme_bw() +
    geom_vline(xintercept = observed_LoD, color = "yellow3") +
    annotate("text", x = observed_LoD, y = 1, label = round(observed_LoD, 2), vjust = 1, hjust = -0.2, color = "yellow3", size = 6) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99)) +
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12)) 
  
  assign(paste0("observed_LoD_", Fspecies), observed_LoD, envir = .GlobalEnv)
  
  print(ggplotcheck)
  
}
