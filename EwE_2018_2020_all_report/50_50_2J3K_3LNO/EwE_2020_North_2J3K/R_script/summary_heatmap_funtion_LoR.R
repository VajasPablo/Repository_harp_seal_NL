
### Summary heatmap of percentage change ###

summary_heatmap_funtion_LoR <- function(data_sp_cod, sp_cod_1, sp_cod_2, nb_row_spcod_1, nb_row_spcod_2,
                                        data_sp_capelin, sp_capelin, nb_row_spcapelin_1, nb_row_spcapelin_2) {
  
  # Création des nouvelles lignes

  new_row_2_1 <- c(Group_name = sp_cod_1, LoR_1 = 0, LoR_10 = 0, LoR_20 = 0, LoR_30 = 0, LoR_40 = 0,
                   LoR_50 = 0, LoR_60 = 0, LoR_obs =0, LoR_70 = 0, LoR_80 = 0, LoR_90 = 0, LoR_100 = 0)
  new_row_2_2 <- c(Group_name = sp_cod_2, LoR_1 = 0, LoR_10 = 0, LoR_20 = 0, LoR_30 = 0, LoR_40 = 0,
                   LoR_50 = 0, LoR_60 = 0, LoR_obs =0, LoR_70 = 0, LoR_80 = 0, LoR_90 = 0, LoR_100 = 0)
  
  new_row_3 <- c(Group_name = sp_capelin, LoR_1 = 0, LoR_10 = 0, LoR_20 = 0, LoR_obs =0, LoR_30 = 0, LoR_40 = 0,
                 LoR_50 = 0, LoR_60 = 0, LoR_70 = 0, LoR_80 = 0, LoR_90 = 0, LoR_100 = 0)
  
  # Insérer les nouvelles lignes aux emplacements spécifiés

  data_sp_cod <- rbind(data_sp_cod[1:nb_row_spcod_1, ], new_row_2_1, new_row_2_2, data_sp_cod[(nb_row_spcod_2):nrow(data_sp_cod), ])
  data_sp_capelin <- rbind(data_sp_capelin[1:nb_row_spcapelin_1, ], new_row_3, data_sp_capelin[(nb_row_spcapelin_2):nrow(data_sp_capelin), ])
  
  # Assigner les noms des espèces aux nouvelles lignes

  data_sp_cod$Species_focus <- sp_cod_1
  data_sp_capelin$Species_focus <- sp_capelin
  
  rownames(data_sp_cod) <- NULL
  rownames(data_sp_capelin) <- NULL
  
  # pivot data
  
  pivot_data_cod <- data_sp_cod %>%
  pivot_longer(cols = starts_with("LoR"), names_to = "LoR", values_to = "Value")
  pivot_data_cod$Group_name <- factor(pivot_data_cod$Group_name, levels = rev(unique(pivot_data_cod$Group_name)))
  pivot_data_cod$Species_focus <- factor(pivot_data_cod$Species_focus, levels = c("Cod> 35 cm"))
  pivot_data_cod$LoR <- gsub("_", " ", pivot_data_cod$LoR)
  pivot_data_cod <- pivot_data_cod %>%
    mutate(LoR = factor(LoR, levels = c("LoR 1", "LoR 10", "LoR 20", "LoR 30", "LoR 40", "LoR 50", "LoR 60", "LoR obs", "LoR 70", "LoR 80", "LoR 90", "LoR 100")))
  
  pivot_data_capelin <- data_sp_capelin %>%
  pivot_longer(cols = starts_with("LoR"), names_to = "LoR", values_to = "Value")
  pivot_data_capelin$Group_name <- factor(pivot_data_capelin$Group_name, levels = rev(unique(pivot_data_capelin$Group_name)))
  pivot_data_capelin$Species_focus <- factor(pivot_data_capelin$Species_focus, levels = c("Cod> 35 cm"))
  pivot_data_capelin$LoR <- gsub("_", " ", pivot_data_capelin$LoR)
  pivot_data_capelin <- pivot_data_capelin %>%
    mutate(LoR = factor(LoR, levels = c("LoR 1", "LoR 10", "LoR 20", "LoR obs", "LoR 30", "LoR 40", "LoR 50", "LoR 60",  "LoR 70", "LoR 80", "LoR 90", "LoR 100")))
  
  # RDS export
  
  current_dir <- getwd()
  project_file <- list.files(current_dir, pattern = "\\.Rproj$", full.names = TRUE)
  project_name <- tools::file_path_sans_ext(basename(project_file))
  
  file_name_cod <- here("Rds_export_adapted_data", paste0("summary_monospe_cod_LoR_", project_name, ".rds"))
  saveRDS(pivot_data_cod, file = file_name_cod)
  
  file_name_capelin <- here("Rds_export_adapted_data", paste0("summary_monospe_capelin_LoR_", project_name, ".rds"))
  saveRDS(pivot_data_capelin, file = file_name_capelin)
  
# Create heatmap 
  
  create_heatmap_summary_cod <- ggplot(pivot_data_cod, aes(x = LoR, y = Group_name, fill = Value)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("#274c77", "#6096ba", "#a3cef1", "#e9c46a", "#f4a261", "#e76f51", "grey95"),
                      breaks = c("- - -", "- -", "-", "+", "+ +", "+ + +", "0"),
                      labels = c("- - -", "- -", "-", "+", "+ +", "+ + +","nc")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = NULL, y = NULL, fill = NULL, title = "Summary of Relative Impact Changes \nBy Cod LoR") +
    coord_fixed() + 
    scale_x_discrete(position = "top") +
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 8),  
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14),  
          legend.text = element_text(size = 9),
          strip.text = element_text(size = 14)) 
  
  create_heatmap_summary_capelin <- ggplot(pivot_data_capelin, aes(x = LoR, y = Group_name, fill = Value)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("#274c77", "#6096ba", "#a3cef1", "#e9c46a", "#f4a261", "#e76f51", "grey95"),
                      breaks = c("- - -", "- -", "-", "+", "+ +", "+ + +", "0"),
                      labels = c("- - -", "- -", "-", "+", "+ +", "+ + +","nc")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = NULL, y = NULL, fill = NULL, title = "Summary of Relative Impact Changes \nBy Capelin LoR") +
    coord_fixed() + 
    scale_x_discrete(position = "top") +
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 8),  
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14),  
          legend.text = element_text(size = 9),
          strip.text = element_text(size = 14)) 
  
print(create_heatmap_summary_cod)
print(create_heatmap_summary_capelin)
  
}
