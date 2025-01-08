
introductive_figure <- function(){ 

# Création des valeurs pour chaque espèce et chaque année
Year <- c(1985, 1985, 1985, 2013, 2013, 2013, 2020, 2020, 2020,
          1985, 1985, 1985, 2013, 2013, 2013, 2020, 2020, 2020)  # Années répétées

Species <- c("Cod", "Capelin", "Harp seal", "Cod", "Capelin", "Harp seal", "Cod", "Capelin", "Harp seal",
             "Squid", "Shrimp", "Snow crab", "Squid", "Shrimp", "Snow crab", "Squid", "Shrimp", "Snow crab")  # Espèces

# Valeurs correspondantes
Values <- c(
  3.576+0.958, 13.77, 0.534*0.4,   # 1985
  0.76+0.038067, 4.97, 0.665*0.4,  # 2013
  (2.18+0.2 + 0.568+0.0521)/2, (3.05 + 3.5)/2, (0.707*0.4 + 0.652*0.4)/2,  # 2020 (2J3K + 3LNO)
  0.365, 0.876, 0.180,   # 1985
  0.365, 2.44, 0.33,     # 2013
  (0.400 + 0.600)/2, (0.584999 + 0.12)/2, (0.241999 + 0.43)/2  # 2020 (2J3K + 3LNO)
)

# Définir l'ordre des espèces et les couleurs spécifiques
species_order <- c("Harp seal", "Cod", "Capelin","Shrimp", "Snow crab", "Squid")
species_colors <- c("Cod" = "lightgreen", 
                    "Capelin" = "green3", 
                    "Harp seal" = "lightblue3", 
                    "Squid" = "coral4", 
                    "Shrimp" = "coral1", 
                    "Snow crab" = "coral3")

# Créer un data frame avec les colonnes Year, Species, et Values
EwE_data <- data.frame(Year, Species, Values)

# Calculer les valeurs normalisées par espèce
EwE_data <- EwE_data %>%
  mutate(Species = factor(Species, levels = species_order)) %>%
  group_by(Species) %>%
  mutate(Rescaled_Values = (Values - min(Values)) / (max(Values) - min(Values)))

# Définir les années à annoter
annotation_years <- c(1985, 2013, 2020)

# Définir les positions pour les annotations
annotation_positions <- data.frame(
  Year = annotation_years,
  Values = rep(max(EwE_data$Values, na.rm = TRUE) + 1, length(annotation_years)),
  Rescaled_Values = rep(max(EwE_data$Rescaled_Values, na.rm = TRUE) + 0.1, length(annotation_years)),
  Year_text = paste0("EwE ", annotation_years)  # Ajouter "EwE" devant les années
)

# Première figure : Valeurs originales avec annotations
p1 <- ggplot(EwE_data, aes(x = Year, y = Values, color = Species)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.5) +
  scale_color_manual(values = species_colors, name = "Key species") +
  labs(x = "Year", y = "t/km2") + #, title = "Original Values by Species") +
  theme_light() +
  #geom_vline(xintercept = annotation_years, color = "red", linetype = "dashed") +
 # geom_text(data = annotation_positions, aes(x = Year, y = Values, label = Year_text), color = "tomato", vjust = 0.5, angle = 90) +
  expand_limits(y = c(0, max(EwE_data$Values, na.rm = TRUE) + 1))

# Deuxième figure : Valeurs rescalées avec annotations
p2 <- ggplot(EwE_data, aes(x = Year, y = Rescaled_Values, color = Species)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.5) +
  scale_color_manual(values = species_colors, name = "Key species") +
  labs(x = "Year", y = "Relative Biomass") + #, title = "Rescaled Values by Species") +
  theme_light() +
  #geom_vline(xintercept = annotation_years, color = "red", linetype = "dashed") +
  #geom_text(data = annotation_positions, aes(x = Year, y = Rescaled_Values, label = Year_text), color = "tomato", vjust = 0.5, angle = 90) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +  # Définir les limites et intervalles
  expand_limits(y = c(0, 1))

# Afficher les deux graphiques

gg_custom <- theme(
  # Axes
  axis.text.x = element_text(size = 12, colour = "black"),  # Taille et couleur des valeurs sur l'axe x
  axis.text.y = element_text(size = 12, colour = "black"),  # Taille et couleur des valeurs sur l'axe y
  axis.title.x = element_text(size = 14, face = "bold"),    # Taille et style du titre de l'axe x
  axis.title.y = element_text(size = 14, face = "bold"),    # Taille et style du titre de l'axe y
  axis.ticks = element_line(size = 0.5),                    # Taille des ticks sur les axes
  
  # Titre
  plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
  
  # Légende
  legend.position = "right",
  legend.background = element_rect(fill = "white", colour = "black"),
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 10),
  
  # Annotations des facettes
  strip.background = element_rect(fill = "gray85"),
  strip.text = element_text(size = 12, face = "bold")
)

# Appliquer le thème personnalisé aux graphiques
p1 <- p1 + gg_custom
p2 <- p2 + gg_custom

# Afficher les deux graphiques avec le thème personnalisé
#grid.arrange(p1, p2, ncol = 2)

# Combiner les deux graphiques en utilisant patchwork et garder une seule légende
combined_plot <- (p1 + p2) + 
  plot_layout(guides = "collect") 

# Afficher la figure combinée
combined_plot

}