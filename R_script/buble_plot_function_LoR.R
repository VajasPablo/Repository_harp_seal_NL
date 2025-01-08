
buble_plot_function_LoR <- function(species, data_output, plot_type) {
  
  LoR_column_names <- paste0("LoR ", c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
  
  process_select_data <- function(data, EwE_period) {
    data %>%
      filter(Group_name == "Cod> 35 cm") %>%
      select(-Group_number) %>%
      select(Group_name, everything()) %>%
      mutate(Group_name = ifelse(Group_name == "Cod> 35 cm", paste0("Cod EwE ", EwE_period), Group_name)) %>%
      rename_with(~ LoR_column_names, starts_with(data_output))
  }
  
  data_objects <- c("2020_2J3K", "2020_3LNO", "2020_merged_2J3K_3LNO")
  
  get_data_object <- function(prefix, period) {
    get(paste0(prefix, data_output, "_final_EwE_", period))
  }
  
  if (species == "Capelin") {
    processed_data <- do.call(rbind, lapply(data_objects, function(period) {
      process_select_data(get_data_object("Df_merged_Bcapelin_", period), period)
    }))
  } else {
    stop("Invalid species argument. Choose 'Capelin'.")
  }
  
  Df_transf <- processed_data %>%
    mutate(Group_name = case_when(
      Group_name == "Cod EwE 2020_2J3K" ~ "Cod EwE 2020 2J3K",
      Group_name == "Cod EwE 2020_3LNO" ~ "Cod EwE 2020 3LNO",
      Group_name == "Cod EwE 2020_merged_2J3K_3LNO" ~ "Cod EwE 2020 merged",
      TRUE ~ Group_name
    )) %>%
    mutate(across(-Group_name, as.numeric)) %>%
    pivot_longer(cols = -Group_name,
                 names_to = "Simulation",
                 values_to = "Value") %>%
    mutate(Value = as.numeric(Value)) %>%
    mutate(Group_name = factor(Group_name, levels = c("Cod EwE 2020 merged", "Cod EwE 2020 3LNO", "Cod EwE 2020 2J3K"))) %>%
    mutate(Simulation = factor(Simulation, levels = LoR_column_names)) %>%
    group_by(Group_name, Simulation) %>%
    mutate(Value_normalized = (Value - min(Value)) / (max(Value) - min(Value))) %>%
    ungroup() %>%
    group_by(Group_name) %>%
    mutate(LoR_1_value = Value[Simulation == "LoR 1"]) %>%
    mutate(Value_normalized = ifelse(Simulation == "LoR 1", 0, (Value - first(LoR_1_value)) / first(LoR_1_value) * 100)) %>%
    ungroup() %>%
    filter(!is.nan(Value_normalized))
  
  if (plot_type %in% c("first", "both")) {
    ggplot1 <- ggplot(Df_transf, aes(x = Simulation, y = Group_name, fill = Value_normalized)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "tomato") +
      theme_minimal() +
      labs(x = paste(species, "LoR simulations"), y = "Cod through EwE time period", fill = data_output) +
      theme_minimal() +
      scale_x_discrete(position = "top") +
      geom_text(aes(label = paste0(round(Value, 2), " t/km2\n", floor(Value_normalized), "%")), size = 3, color = "black") +
      theme(axis.text.x = element_text(angle = 20, hjust = 0)) +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 16),
            legend.text = element_text(size = 12))
  } else {
    ggplot1 <- NULL
  }
  
  if (plot_type %in% c("second", "both")) {
    min_value <- min(Df_transf$Value, na.rm = TRUE)
    mean_value <- mean(Df_transf$Value, na.rm = TRUE)
    max_value <- max(Df_transf$Value, na.rm = TRUE)
    
    ggplot2 <- ggplot(Df_transf, aes(x = Simulation, y = Group_name, color = Value_normalized, size = Value)) +
      geom_point() +
      scale_color_gradient(low = "gray95", high = "black") +
      scale_size(range = c(5, 30), breaks = c(min_value, mean_value, max_value)) +
      theme_minimal() +
      labs(x = paste(species, "LoR simulations"), y = "Cod through EwE time period", color = paste("% change", data_output), size = paste(data_output, "t/km2")) +
      scale_x_discrete(position = "top") +
      theme(axis.text.x = element_text(angle = 20, hjust = 0),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            panel.grid = element_blank()) +
      guides(size = guide_legend(order = 1))
  } else {
    ggplot2 <- NULL
  }
  
  if (plot_type == "both") {
    grid.arrange(ggplot1, ggplot2, ncol = 1)
  } else if (plot_type == "first") {
    print(ggplot1)
  } else if (plot_type == "second") {
    print(ggplot2)
  } else {
    stop("Invalid plot_type argument. Choose from 'first', 'second', or 'both'.")
  }
}
