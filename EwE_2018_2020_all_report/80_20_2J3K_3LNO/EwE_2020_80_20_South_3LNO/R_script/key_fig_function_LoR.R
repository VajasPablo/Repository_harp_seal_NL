
### Key figure function ###

key_fig_function_LoR <- function(LoR_order, catch_data, Group_number_specific, data_20, data_40, obs_LoR_data, x_axis_species, name_species_rds) {

  column_names <- paste0("LoR_", LoR_order)
  
  Df_catch_msy <- catch_data %>%
    filter(Group_number == Group_number_specific) %>%
    select(- c(Group_number, Group_name)) %>%
    rename_with(~ column_names, starts_with("c")) %>%
    pivot_longer(cols = starts_with("LoR"), names_to = "LoR", values_to = "Catch_value") %>%
    mutate(Catch_value = as.numeric(Catch_value)) %>%
    mutate(Catch_rate = Catch_value/max(Catch_value)) %>%
    mutate(LoR = gsub("LoR_", "", LoR)) %>% 
    mutate(LoR_factor = factor(LoR, levels = LoR_order)) %>%
    mutate(LoR = as.numeric(LoR_factor))
  
  process_data <- function(data, suffix) {
    data %>%
      filter(Description == "Total Prop. ±") %>%
      select(-Description) %>%
      pivot_longer(cols = starts_with("LoR"), names_to = "LoR", values_to = paste0("value", suffix)) %>%
      mutate(LoR = gsub("LoR_", "", LoR),
             value = as.numeric(get(paste0("value", suffix)))) %>% 
      select(-value) %>% 
        mutate(LoR = factor(LoR, levels = LoR_order))
  }
  
  long_table_20 <- process_data(data_20, "20")
  long_table_40 <- process_data(data_40, "40")
  
  pourcent_group_impacted_20_40 <- left_join(long_table_20, long_table_40, by = "LoR")
  
  ggplot_data <- pourcent_group_impacted_20_40 %>%
    pivot_longer(cols = c(value20, value40), names_to = "Variable", values_to = "Value") 
  
  # RDS export
  current_dir <- getwd()
  project_file <- list.files(current_dir, pattern = "\\.Rproj$", full.names = TRUE)
  project_name <- tools::file_path_sans_ext(basename(project_file))
  file_name <- here("Rds_export_adapted_data", paste0(name_species_rds, "_Bsim_GrpImpact_", project_name, ".rds"))
  saveRDS(ggplot_data, file = file_name)
  file_name <- here("Rds_export_adapted_data", paste0(name_species_rds, "_Bsim_catch_", project_name, ".rds"))
  saveRDS(Df_catch_msy, file = file_name)
  
  keyplot <- ggplot(data = ggplot_data, aes(x = LoR, y = Value, color = Variable)) +
    geom_line(aes(group = Variable), size = 1) +
    geom_point(size = 0) + 
    geom_smooth(data = Df_catch_msy, aes(x = LoR, y = Catch_rate),
               method = "gam",   
               se = FALSE, 
               color = "yellow3") +
    scale_color_manual(values = c("lightblue4", "tomato"),
                       breaks = c("value20", "value40"),
                       labels = c("Upper 20%", "Upper 40%")) +
    geom_segment(aes(x = obs_LoR_data, xend = obs_LoR_data, y = 0.30, yend = 0),
                 arrow = arrow(length = unit(0.5, "cm")), color = "black", size = 1) +
    guides(color = guide_legend(title = "Prop. Group Impacted")) +
    scale_y_continuous(name = "Prop. impacted group", breaks = seq(0, 1, 0.25),   
                       labels = seq(0, 1, 0.25), expand = c(0, 0),
                       sec.axis = sec_axis(~ ., name = "MSY",
                                           breaks = seq(0, 1, 0.25),   
                                           labels = seq(0, 1, 0.25))) +
    labs(x = paste("LoR", x_axis_species)) +
    theme_bw() +
    coord_cartesian(ylim = c(0, 1.05)) +
    theme(axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14), 
          axis.text.x = element_text(size = 12),  
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),  
          legend.text = element_text(size = 12))
  
  print(keyplot)
  
}
