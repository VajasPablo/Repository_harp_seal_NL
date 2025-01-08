Cod_trends_LoR_capelin_function <- function() {
  
  LoR_order = c(1, 10, 20, "obs", 30, 40, 50, 60, 70, 80, 90, 100)
  
  # Définir les seuils de biomasse pour les différents groupes
  Biomass_cod_sup35_1985 <- 3.576
  Biomass_thresholds <- c(Critical = 0.25 * Biomass_cod_sup35_1985, 
                          Cautious = 0.85 * Biomass_cod_sup35_1985)
  
  # Conversion des données et préparation
  Bcapelin_LoR_Cod_biomass_2J3K_50_mutate <- Bcapelin_LoR_Cod_biomass_2J3K_50 %>% mutate(Group_name = paste("2J3K 50%:", Group_name))
  Bcapelin_LoR_Cod_biomass_3LNO_50_mutate <- Bcapelin_LoR_Cod_biomass_3LNO_50 %>% mutate(Group_name = paste("3LNO 50%:", Group_name))
  Bcapelin_LoR_Cod_biomass_2J3K_80_mutate <- Bcapelin_LoR_Cod_biomass_2J3K_80 %>% mutate(Group_name = paste("2J3K 80%:", Group_name))
  Bcapelin_LoR_Cod_biomass_3LNO_20_mutate <- Bcapelin_LoR_Cod_biomass_3LNO_20 %>% mutate(Group_name = paste("3LNO 20%:", Group_name))
  
  df <- rbind(Bcapelin_LoR_Cod_biomass_2J3K_50_mutate, 
              Bcapelin_LoR_Cod_biomass_3LNO_50_mutate, 
              Bcapelin_LoR_Cod_biomass_2J3K_80_mutate, 
              Bcapelin_LoR_Cod_biomass_3LNO_20_mutate) %>%
    mutate(Simulation = str_remove(Simulation, "LoR ")) %>%
    mutate(Simulation = factor(Simulation, levels = LoR_order))
  
  # Séparer les données pour chaque groupe
  N_cod_sup35 <- df %>% filter(Group_name == "2J3K 50%: Cod> 35 cm")
  S_cod_sup35 <- df %>% filter(Group_name == "3LNO 50%: Cod> 35 cm")
  N_s_cod_sup35 <- df %>% filter(Group_name == "2J3K 80%: Cod> 35 cm")
  S_s_cod_sup35 <- df %>% filter(Group_name == "3LNO 20%: Cod> 35 cm")
  
  # Créer la figure avec plusieurs groupes
  plotline <- ggplot() + 
    # Bandes colorées avec légende et ordre personnalisé
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Biomass_thresholds["Critical"], fill = "Critical"), 
              alpha = 0.60, inherit.aes = FALSE) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = Biomass_thresholds["Critical"], ymax = Biomass_thresholds["Cautious"], fill = "Cautious"), 
              alpha = 0.60, inherit.aes = FALSE) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = Biomass_thresholds["Cautious"], ymax = Inf, fill = "Healthy"), 
              alpha = 0.60, inherit.aes = FALSE) +
    
    # Ajouter les courbes activées
    geom_line(data = N_cod_sup35, aes(x = as.factor(Simulation), y = Value, color = "2J3K 50%: Cod> 35 cm", group = Group_name), size = 1) +
    geom_point(data = N_cod_sup35, aes(x = as.factor(Simulation), y = Value, color = "2J3K 50%: Cod> 35 cm"), size = 3, alpha = 0.8) +
    
    geom_line(data = S_cod_sup35, aes(x = as.factor(Simulation), y = Value, color = "3LNO 50%: Cod> 35 cm", group = Group_name), size = 1) +
    geom_point(data = S_cod_sup35, aes(x = as.factor(Simulation), y = Value, color = "3LNO 50%: Cod> 35 cm"), size = 3, alpha = 0.8) +
    
    geom_line(data = N_s_cod_sup35, aes(x = as.factor(Simulation), y = Value, color = "2J3K 80%: Cod> 35 cm", group = Group_name), size = 1) +
    geom_point(data = N_s_cod_sup35, aes(x = as.factor(Simulation), y = Value, color = "2J3K 80%: Cod> 35 cm"), size = 3, alpha = 0.8) +
    
    geom_line(data = S_s_cod_sup35, aes(x = as.factor(Simulation), y = Value, color = "3LNO 20%: Cod> 35 cm", group = Group_name), size = 1) +
    geom_point(data = S_s_cod_sup35, aes(x = as.factor(Simulation), y = Value, color = "3LNO 20%: Cod> 35 cm"), size = 3, alpha = 0.8) +
    
    # Échelles des couleurs et des remplissages
    scale_color_manual(values = c(
      "2J3K 50%: Cod> 35 cm" = "#696969",
      "3LNO 50%: Cod> 35 cm" = "#1E95FF",
      "2J3K 80%: Cod> 35 cm" = "#212529",
      "3LNO 20%: Cod> 35 cm" = "#1E50FF"
    )) +
    scale_fill_manual(values = c(
      "Critical" = "#FF6347",
      "Cautious" = "#FFA500",
      "Healthy" = "#008000"
    ), 
    limits = c("Healthy", "Cautious", "Critical"), # Ordre personnalisé
    name = "Status") +
    
    # Labels
    labs(x = "Capelin Level of Recovery (%)",
         y = "Cod Biomass (t/km2)", 
         color = "Area/Sensitivity",
         fill = "Status") +
    
    # Réorganiser les légendes
    guides(
      color = guide_legend(order = 1), # Area/Sensitivity en premier
      fill = guide_legend(order = 2)   # Status en second
    ) +
    
    # Personnalisation du thème
    theme_light() +
    theme(
      axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 16),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 13),
      legend.position = "right" # Place la légende à droite
    )
  
  print(plotline)
}