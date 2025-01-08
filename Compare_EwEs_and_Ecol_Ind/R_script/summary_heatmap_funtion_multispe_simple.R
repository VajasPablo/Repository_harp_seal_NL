
### Summary multispecific heatmap of percentage change ###

summary_heatmap_funtion_multi <- function(data, species_one, species_two) {
  
  lod_levels_1 <- c(paste0("LoD ", species_one, " obs"),
                  paste0("LoD ", species_one, " 0"),
                  paste0("LoD ", species_one, " 25"),
                  paste0("LoD ", species_one, " 50"),
                  paste0("LoD ", species_one, " 75"),
                  paste0("LoD ", species_one, " 99"))
  
  lod_levels_2 <- c(paste0("LoD ", species_two, " obs"),
                    paste0("LoD ", species_two, " 0"),
                    paste0("LoD ", species_two, " 25"),
                    paste0("LoD ", species_two, " 50"),
                    paste0("LoD ", species_two, " 75"),
                    paste0("LoD ", species_two, " 99"))
  
  pivot_longer_data <- data %>%
    pivot_longer(cols = starts_with("LoD"),
                 names_to = c(paste0("LoD_", species_one), paste0("LoD_", species_two)),
                 names_pattern = paste0("LoD_", species_one, "_(.*?)_LoD_", species_two, "_(.*)"),
                 values_to =  "value") %>% 
    mutate_at(vars(starts_with(paste0("LoD_", species_one))), list(~ paste("LoD", species_one, .))) %>%
    mutate_at(vars(starts_with(paste0("LoD_", species_two))), list(~ paste("LoD", species_two, .))) %>%
    mutate(
      !!paste0("LoD_", species_one) := factor(get(paste0("LoD_", species_one)), levels = lod_levels_1),
      !!paste0("LoD_", species_two) := factor(get(paste0("LoD_", species_two)), levels = lod_levels_2)) 
  
  pivot_longer_data$Group_name <- factor(pivot_longer_data$Group_name, levels = rev(unique(pivot_longer_data$Group_name)))
  
  # RDS export
  current_dir <- getwd()
  project_file <- list.files(current_dir, pattern = "\\.Rproj$", full.names = TRUE)
  project_name <- tools::file_path_sans_ext(basename(project_file))
  file_name <- here("Rds_export_adapted_data", paste0("summary_multispe_", project_name, ".rds"))
  saveRDS(pivot_longer_data, file = file_name)

  ggplot(pivot_longer_data, aes_string(x = paste0("LoD_", species_one), y = "Group_name", fill = "value")) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("#274c77", "#6096ba", "#a3cef1", "#e9c46a", "#f4a261", "#e76f51", "grey95"),
                      breaks = c("- - -", "- -", "-", "+", "+ +", "+ + +", "0"),
                      labels = c("- - -", "- -", "-", "+", "+ +", "+ + +","nc"))  +
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
          strip.text = element_text(size = 10)) +
    facet_wrap(~ get(paste0("LoD_", species_two)), ncol = 6, strip.position = "bottom")
}