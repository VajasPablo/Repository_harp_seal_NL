
Harp_seal_relative_MSE_simulation_function_3LNO <- function(data_simulation, data_assessment){
  
  data_process <- data_simulation %>% 
    filter(Group_number %in% c(5)) %>%
    select(-c(Group_number, Group_name)) %>%
    mutate(across(everything(), as.numeric)) %>%
    summarise_all(.funs = sum) %>%
    pivot_longer(cols = everything(), 
                 names_to = c("LoR_cod", "LoR_capelin"),
                 names_pattern = "LoR_cod_(.*?)_LoR_capelin_(.*)",
                 values_to = "biomass_seal") %>%
    mutate(across(starts_with("biomass_"), as.numeric)) %>%
    mutate(
      LoR_cod = factor(LoR_cod, levels = c("1", "25", "50", "obs", "75", "100")),
      LoR_capelin = factor(LoR_capelin, levels = c("100", "75", "50", "25", "obs", "1"))) %>% 
    mutate(seal_biomass_change = if_else(LoR_capelin == "obs" & LoR_cod == "obs", 
                                         0, 
                                         round((pull(., "biomass_seal")[LoR_capelin == "obs" & LoR_cod == "obs"] - pull(., "biomass_seal")) / 
                                                 pull(., "biomass_seal")[LoR_capelin == "obs" & LoR_cod == "obs"], 5) * -100)) %>%
    filter(LoR_cod == "1" & LoR_capelin == "1" |
             LoR_cod == "25" & LoR_capelin == "obs" |
             LoR_cod == "50" & LoR_capelin == "25" |
             LoR_cod == "obs" & LoR_capelin == "50" |
             LoR_cod == "75" & LoR_capelin == "75" |
             LoR_cod == "100" & LoR_capelin == "100" |
             LoR_cod == "obs" & LoR_capelin == "obs") %>% 
    mutate(LoR_cod = paste0("LoR_Cod_", LoR_cod),
           LoR_capelin = paste0("LoR_Capelin_", LoR_capelin)) %>% 
    mutate(Harp_seal_total_biomass_3LNO = biomass_seal*257400) %>%
    mutate(Year = 2035) %>%   mutate(
      LoR_cod = str_replace_all(LoR_cod, "_", " "),
      LoR_capelin = str_replace_all(LoR_capelin, "_", " ")
    ) %>%
    mutate(
      LoR_cod = factor(LoR_cod, levels = c("LoR Cod 1", "LoR Cod obs", "LoR Cod 25", "LoR Cod 50", "LoR Cod 75", "LoR Cod 100")),
      LoR_capelin = factor(LoR_capelin, levels = c("LoR Capelin 1", "LoR Capelin obs", "LoR Capelin 25", "LoR Capelin 50", "LoR Capelin 75", "LoR Capelin 100"))
    )
  
  # Normaliser les données de data_assessment en fixant l'année 2020 à 1
  max_value_stock_assessment <- data_assessment %>%
    filter(Year == 2020) %>%
    pull(Biomass_T_area_rescale)
  
  data_assessment <- data_assessment %>%
    mutate(Biomass_T_area_rescale = if_else(Year == 2020, 1, Biomass_T_area_rescale / max_value_stock_assessment))
  
  # Ajustement des valeurs de Harp_seal_total_biomass_3LNO pour correspondre à la normalisation
  data_process <- data_process %>%
    mutate(Harp_seal_total_biomass_3LNO = Harp_seal_total_biomass_3LNO / max_value_stock_assessment)
  
  #print(data_process)
  
  gg <- ggplot(data_assessment, aes(x = Year, y = Biomass_T_area_rescale)) +
    geom_line() + 
    labs(x = "Year", y = "Biomass_T_area_rescale")+
    geom_point(data = data_process[data_process$Year == 2035, ], 
               aes(x = Year, y = Harp_seal_total_biomass_3LNO, color = LoR_cod, shape = LoR_capelin),  
               size = 4, show.legend = TRUE) +  
    scale_color_manual(values = c("LoR Cod obs" = "blue", "LoR Cod 25" = "green", "LoR Cod 1" = "red", "LoR Cod 50" = "orange", "LoR Cod 75" = "purple", "LoR Cod 100" = "pink")) +
    scale_shape_manual(values = c("LoR Capelin obs" = 0, "LoR Capelin 25" = 1, "LoR Capelin 1" = 2, "LoR Capelin 50" = 3, "LoR Capelin 75" = 4, "LoR Capelin 100" = 5)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    geom_hline(yintercept = max(data_assessment$Biomass_T_area_rescale) * 1.6, linetype = "dashed", color = "yellow4") +
    scale_y_continuous(
      limits = c(0, 3),
      breaks = seq(0, 3, by = 0.5),
      labels = scales::label_number(scale = 1, suffix = "", big.mark = ",")
    ) +
    theme_bw() +
    labs(shape = "LoR Capelin", color = "LoR Cod") +
    guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 10)) + 
    ylab("Relative Harp Seal Biomass Change") + 
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 0.999), color = "blue", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 1.09), color = "green", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 0.873), color = "red", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 1.26), color = "orange", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 1.33), color = "blue", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 2.20), color = "purple", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 2.82), color = "pink", size = 0.05, show.legend = FALSE) 
  
  print(gg)
  
}

