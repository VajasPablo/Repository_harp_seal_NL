
key_fig_function_LoR_web <- function(LoR_order, data_20, data_40, obs_LoR_data, x_axis_species, name_species_rds) {
  
  #column_names <- paste0("LoR_", LoR_order)
  
  process_data <- function(data, suffix) {
    first_transformation <- data %>%
      filter(Description == "Prop. of +" | Description == "Prop. of -") %>%
      pivot_longer(cols = starts_with("LoR"), names_to = "LoR", values_to = "value") %>%
      mutate(LoR = gsub("LoR_", "", LoR),
             value = as.numeric(value),
             value = ifelse(Description == "Prop. of -", -value, value)) %>% 
      mutate(LoR = factor(LoR, levels = LoR_order)) %>%
      rename_with(~paste0(.x, suffix), starts_with("value"))
    
    neg_value <- first_transformation %>% filter(Description == "Prop. of -") %>% select(-Description) %>% rename_with(~paste0(.x, "_neg"), starts_with("value"))
    pos_value <- first_transformation %>% filter(Description == "Prop. of +") %>% select(-Description) %>% rename_with(~paste0(.x, "_pos"), starts_with("value"))
    
    left_join(neg_value, pos_value, by = "LoR")
  }
  
  long_table_20 <- process_data(data_20, "20")
  long_table_40 <- process_data(data_40, "40")
  
  pourcent_group_impacted_20_40 <- left_join(long_table_20, long_table_40, by = "LoR")
  
  # RDS export
  current_dir <- getwd()
  project_file <- list.files(current_dir, pattern = "\\.Rproj$", full.names = TRUE)
  project_name <- tools::file_path_sans_ext(basename(project_file))
  file_name <- here("Rds_export_adapted_data", paste0(name_species_rds, "_web_Bsim_GrpImpact_", project_name, ".rds"))
  saveRDS(pourcent_group_impacted_20_40, file = file_name)

  keyplot <- ggplot(pourcent_group_impacted_20_40, aes(x=LoR)) +
    geom_ribbon(aes(x = as.numeric(LoR), ymin = value20_neg, ymax = value20_pos, fill = "Upper 20%"), alpha = 0.8, color = NA) +
    geom_ribbon(aes(x = as.numeric(LoR), ymin = value40_neg, ymax = value40_pos, fill = "Upper 40%"), alpha = 0.8, color = NA) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
    geom_segment(data = pourcent_group_impacted_20_40, aes(x = obs_LoR_data, xend = obs_LoR_data, y = 0.15, yend = 0),
                 arrow = arrow(length = unit(0.5, "cm")), color = "black", size = 1) +
    guides(fill = guide_legend(title = "Prop. Group Impacted")) +
    scale_y_continuous(name = "Prop. impacted group", breaks = seq(-0.5, 0.5, 0.1),
                       labels = seq(-0.5, 0.5, 0.1), expand = c(0, 0), limits = c(-0.5, 0.5)) +
    scale_x_continuous(breaks = seq(1, 12, 1),
                       labels = LoR_order,
                       expand = expansion(add = c(0, 0))) +
    coord_cartesian(xlim = c(1, 12)) +
    labs(x = paste("LoR", x_axis_species)) +
    scale_fill_manual(values = c("lightblue", "tomato"),
                      labels = c("Upper 20%", "Upper 40%")) +
    theme_bw() +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12))
  
  print(keyplot)
  
}
