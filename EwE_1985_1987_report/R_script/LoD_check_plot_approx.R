
### Function in order to generate LoD plot

# LoD_check_plot function for generating a plot
LoD_check_plot_approx <- function(data, observed_biomass, Fspecies, species, species_chrc, Group_number_specific) {
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
  F0 <- data %>%
    filter(Group_number %in% Group_number_specific) %>%
    pull(paste0("biomass_annual_", Fspecies, "_0")) %>%
    as.numeric()
  
  observed_LoD <- ((F0 - observed_biomass) / F0) * 100
  
  # Plot
  obsggplot <- ggplot(LoD, aes(x = F_depletion_value, y = F_sim)) +
    geom_point() +
    labs(x = "Level of Depletion", y = "F rate", title = paste0(species, ": Plot of % depletion vs. F rate")) +
    theme_bw() +
    geom_vline(xintercept = observed_LoD, color = "yellow3") +
    annotate("text", x = observed_LoD, y = 1, label = round(observed_LoD, 2), vjust = 1, hjust = -0.2, color = "yellow3", size = 6) +
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12)) 
  
  # INTERPOLATED VALUE PART
  LoD_fixe <- c(0.00, 10.00, 20.00, 25.00, 30.00, 40.00, 50.00, 60.00, 70.00, 75.00, 80.00, 90.00, 99.00)
  
  LoD_numeric <- LoD %>% select(F_sim, F_depletion_value) %>% mutate(F_sim = as.numeric(gsub("[^0-9.]+", "", F_sim)))
  
  # Apply the function for each specified LoD value
  result_df <- do.call(rbind, lapply(LoD_fixe, function(x) {
    interpolated_values <- approx(LoD_numeric$F_depletion_value, LoD_numeric$F_sim, xout = x)$y
    data.frame(Fixed_LoD = x, F_mortality_interpolated = interpolated_values)
  })) 
  
  # Create the plot with ggplot
  approxggplot <- ggplot(LoD_numeric, aes(x = F_depletion_value, y = F_sim)) +
    geom_point(color = "darkblue") +
    geom_line(aes(group = 1), linetype = "solid", color = "gray") +  
    geom_point(data = result_df, aes(x = Fixed_LoD, y = F_mortality_interpolated), color = "red") +
    geom_line(data = result_df, aes(x = Fixed_LoD, y = F_mortality_interpolated), linetype = "dashed", color = "red") +
    labs(x = "Level of Depletion", y = "F rate", 
         title = paste0(species, ": Plot of % depletion vs. F rate with \nin red approximation results")) +
    theme_bw() + 
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12)) 
  
  # Print the plot
  grid.arrange(obsggplot, approxggplot, ncol = 2)
  
  # Assign the result to global environment
  assign(paste0("Df_LoD_fixed_", species_chrc), result_df, envir = .GlobalEnv)
}
