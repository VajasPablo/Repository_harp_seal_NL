
Cod_trends_LoR_capelin_function <- function(data, LoR_order, species_name, species_select, area, save_name){
  
  Biomass_cod_sup35_1985 <- 3.576
  Biomass_cod_inf35_1985 <- 0.958
  
  # Conversion des données et préparation
  column_names <- paste0("LoR ", LoR_order)
  
  df <- data %>%
    rename_with(~ gsub("biomass_annual_", "", .), starts_with("biomass_annual_")) %>%
    rename_with(~ column_names, starts_with("B")) %>%
    select(-1) %>%
    mutate(across(-Group_name, as.numeric)) %>%
    pivot_longer(cols = -Group_name,
                 names_to = "Simulation",
                 values_to = "Value") %>%
    mutate(Value = as.numeric(Value)) %>%
    mutate(Group_name = factor(Group_name, levels = rev(unique(Group_name)))) %>%
    filter(Group_name %in% species_select) %>%
    mutate(Simulation = factor(Simulation, levels = column_names))
  
  # Séparer les données pour chaque groupe de poissons
  df_cod_sup35 <- df %>% filter(Group_name == "Cod> 35 cm")
  df_cod_inf35 <- df %>% filter(Group_name == "Cod<= 35 cm")
  
  # Créer la figure avec un seul y-axis pour les deux groupes
  plotline <- ggplot() + 
    # Ajout des données pour "Cod> 35 cm"
    geom_line(data = df_cod_sup35, aes(x = as.factor(Simulation), y = Value, group = 1, color = "Cod> 35 cm"), size = 1) +
    geom_point(data = df_cod_sup35, aes(x = as.factor(Simulation), y = Value, 
                                        color = ifelse(Value > Biomass_cod_sup35_1985, ">= 1985 biomass", "Cod> 35 cm")), 
               size = ifelse(df_cod_sup35$Value > Biomass_cod_sup35_1985, 4, 3)) +  # Points plus gros si dépassent le seuil
    
    # Ajout des données pour "Cod<= 35 cm"
    geom_line(data = df_cod_inf35, aes(x = as.factor(Simulation), y = Value, group = 1, color = "Cod<= 35 cm"), size = 1) +
    geom_point(data = df_cod_inf35, aes(x = as.factor(Simulation), y = Value, 
                                        color = ifelse(Value > Biomass_cod_inf35_1985, ">= 1985 biomass", "Cod<= 35 cm")), 
               size = ifelse(df_cod_inf35$Value > Biomass_cod_inf35_1985, 4, 3)) +  # Points plus gros si dépassent le seuil
    
    # Échelles des couleurs pour les groupes et dépassements
    scale_color_manual(values = c("Cod> 35 cm" = "lightblue3", 
                                  "Cod<= 35 cm" = "orange2", 
                                  ">= 1985 biomass" = "tomato")) +
    
    # Définir les labels des axes
    labs(x = "Level of Restoration simulations",
         y = "Cod Biomass", color = "Group Name") +
    
    # Personnalisation du thème
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 1),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 16),
      legend.text = element_text(size = 12)
    ) +
    
    ggtitle(paste(area, species_name, "LoR simulations"))
  
  print(plotline)
  
  file_name_here <- here("Rds_export_adapted_data", save_name)
  saveRDS(df, file = file_name_here)
}
