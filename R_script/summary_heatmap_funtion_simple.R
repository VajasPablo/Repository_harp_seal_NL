
### Summary heatmap of percentage change ###

summary_heatmap_funtion <- function(data_sp_seal, sp_seal, nb_row_spseal_1, nb_row_spseal_2,
                          data_sp_cod, sp_cod_1, sp_cod_2, nb_row_spcod_1, nb_row_spcod_2,
                          data_sp_capelin, sp_capelin, nb_row_spcapelin_1, nb_row_spcapelin_2) {
  
  # Création des nouvelles lignes
  new_row_1 <- c(Group_name = sp_seal, LoD_0 = 0, LoD_10 = 0, LoD_20 = 0, LoD_30 = 0, LoD_40 = 0,
                 LoD_50 = 0, LoD_60 = 0, LoD_70 = 0, LoD_80 = 0, LoD_90 = 0, LoD_99 = 0)
  new_row_2_1 <- c(Group_name = sp_cod_1, LoD_0 = 0, LoD_10 = 0, LoD_20 = 0, LoD_30 = 0, LoD_40 = 0,
                   LoD_50 = 0, LoD_60 = 0, LoD_70 = 0, LoD_80 = 0, LoD_90 = 0, LoD_99 = 0)
  new_row_2_2 <- c(Group_name = sp_cod_2, LoD_0 = 0, LoD_10 = 0, LoD_20 = 0, LoD_30 = 0, LoD_40 = 0,
                   LoD_50 = 0, LoD_60 = 0, LoD_70 = 0, LoD_80 = 0, LoD_90 = 0, LoD_99 = 0)
  new_row_3 <- c(Group_name = sp_capelin, LoD_0 = 0, LoD_10 = 0, LoD_20 = 0, LoD_30 = 0, LoD_40 = 0,
                 LoD_50 = 0, LoD_60 = 0, LoD_70 = 0, LoD_80 = 0, LoD_90 = 0, LoD_99 = 0)
  
  # Insérer les nouvelles lignes aux emplacements spécifiés
  data_sp_seal <- rbind(data_sp_seal[1:nb_row_spseal_1, ], new_row_1, data_sp_seal[(nb_row_spseal_2):nrow(data_sp_seal), ])
  data_sp_cod <- rbind(data_sp_cod[1:nb_row_spcod_1, ], new_row_2_1, new_row_2_2, data_sp_cod[(nb_row_spcod_2):nrow(data_sp_cod), ])
  data_sp_capelin <- rbind(data_sp_capelin[1:nb_row_spcapelin_1, ], new_row_3, data_sp_capelin[(nb_row_spcapelin_2):nrow(data_sp_capelin), ])
  
  # Assigner les noms des espèces aux nouvelles lignes
  data_sp_seal$Species_focus <- sp_seal
  data_sp_cod$Species_focus <- sp_cod_1
  data_sp_capelin$Species_focus <- sp_capelin
  
  # Fusionner les données
  merged_change_data <- rbind(data_sp_seal, data_sp_cod, data_sp_capelin)
  rownames(merged_change_data) <- NULL

  pivot_data <- merged_change_data %>%
    pivot_longer(cols = starts_with("LoD"), names_to = "LoD", values_to = "Value")
  
  pivot_data$Group_name <- factor(pivot_data$Group_name, levels = rev(unique(pivot_data$Group_name)))
  pivot_data$Species_focus <- factor(pivot_data$Species_focus, levels = c("Seal Harp", "Cod> 35 cm", "Capelin"))
  pivot_data$LoD <- gsub("_", " ", pivot_data$LoD)
  
  # RDS export
  current_dir <- getwd()
  project_file <- list.files(current_dir, pattern = "\\.Rproj$", full.names = TRUE)
  project_name <- tools::file_path_sans_ext(basename(project_file))
  file_name <- here("Rds_export_adapted_data", paste0("summary_monospe_", project_name, ".rds"))
  saveRDS(pivot_data, file = file_name)

   create_heatmap_summary <- ggplot(pivot_data, aes(x = LoD, y = Group_name, fill = Value)) +
     geom_tile(color = "white") +
     scale_fill_manual(values = c("#274c77", "#6096ba", "#a3cef1", "#e9c46a", "#f4a261", "#e76f51", "grey95"),
                       breaks = c("- - -", "- -", "-", "+", "+ +", "+ + +", "0"),
                       labels = c("- - -", "- -", "-", "+", "+ +", "+ + +","nc")) +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     labs(x = NULL, y = NULL, fill = NULL, title = "Summary of Relative Impact Changes \nAcross Study Species") +
     coord_fixed() + 
     scale_x_discrete(position = "top") +
   theme(axis.title.x = element_text(size = 14), 
         axis.title.y = element_text(size = 14), 
         axis.text.x = element_text(size = 8),  
         axis.text.y = element_text(size = 10),
         plot.title = element_text(size = 16),  
         legend.text = element_text(size = 9),
         strip.text = element_text(size = 14)) +
     facet_wrap(~ Species_focus)
   
   print(create_heatmap_summary)

}
