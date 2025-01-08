

Harp_seal_MSE_simulation_function_3LNO_ajust <- function(prop_sensitivity, region, data_simulation, title){
  
  # Internal function to calculate the biomass stock assessment rescale value
  calc_biomass_stock_assessment_rescale <- function(prop_sensitivity, region){
    
    size_area_2J3K <- 237600 # Km2
    size_area_3LNO <- 257400 # Km2
    biomass_2J3K <- 0.700*0.400*size_area_2J3K
    biomass_3LNO <- 0.652*0.400*size_area_3LNO
    
    biomass_area <- if (region == "2J3K") {
      biomass_2J3K
    } else if (region == "3LNO") {
      biomass_3LNO
    } else {
      stop("Invalid region specified. Use '2J3K' or '3LNO'.")
    }
    
    heap_seal_biomass_stock_assessment <- data.frame(
      Year = 1952:2020,
      Biomass_KT = c(
        215.75216, 206.83376, 207.54135, 205.13917, 202.74817, 200.00081, 195.42937, 184.37727, 171.21381, 163.79567,
        170.81837, 162.32686, 156.72836, 146.90643, 146.19302, 141.56015, 135.79332, 136.12596, 130.72090, 130.26264,
        133.78305, 140.63755, 146.68705, 153.14849, 158.65561, 164.59354, 176.93040, 175.36202, 183.92108, 192.61931,
        196.01422, 203.86691, 218.84359, 238.13949, 262.64679, 292.71900, 319.13289, 345.33651, 371.11745, 408.83246,
        446.05849, 466.85569, 496.91703, 516.55996, 467.74122, 473.60281, 467.43739, 455.77551, 467.64967, 448.82977,
        434.35030, 435.06443, 399.56090, 392.25324, 374.07677, 379.65071, 374.28378, 348.58645, 315.34675, 299.58282,
        316.57978, 326.11249, 336.89376, 324.38447, 333.44639, 330.81003, 337.08297, 336.04705, 335.01113
      )
    ) %>%
      mutate(
        Biomass_T = Biomass_KT * 1000,
        Biomass_T_area = Biomass_T * prop_sensitivity
      )
    
    rescale_value <- heap_seal_biomass_stock_assessment %>% 
      filter(Year == 2020) %>% select(Biomass_T_area) %>% 
      pull() / biomass_area 
    
    heap_seal_biomass_stock_assessment <- heap_seal_biomass_stock_assessment %>%  
      mutate(Biomass_T_area_rescale = Biomass_T_area / rescale_value)
    
    return(heap_seal_biomass_stock_assessment)
  }
  
  # Calculate biomass stock assessment rescale value
  data_assessment <- calc_biomass_stock_assessment_rescale(prop_sensitivity, region)
  
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
      LoR_cod = factor(LoR_cod, levels = c("1", "obs", "25", "50", "75", "100")),
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
      LoR_cod = factor(LoR_cod, levels = c("LoR Cod obs", "LoR Cod 1", "LoR Cod 25", "LoR Cod 50", "LoR Cod 75", "LoR Cod 100")),
      LoR_capelin = factor(LoR_capelin, levels = c("LoR Capelin obs", "LoR Capelin 1", "LoR Capelin 25", "LoR Capelin 50", "LoR Capelin 75", "LoR Capelin 100"))
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
               size = 3.5, show.legend = TRUE) +  
    scale_color_manual(values = c("LoR Cod obs" = "#1f77b4", "LoR Cod 25" = "#2ca02c", "LoR Cod 1" = "#d62728", "LoR Cod 50" = "#ff7f0e", "LoR Cod 75" = "#9467bd", "LoR Cod 100" = "#e377c2")) +
    scale_shape_manual(values = c("LoR Capelin obs" = 16, "LoR Capelin 25" = 17, "LoR Capelin 1" = 18, "LoR Capelin 50" = 15, "LoR Capelin 75" = 13, "LoR Capelin 100" = 8)) +
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
    ylab("Relative Harp Seal Biomass Change") + ggtitle(title) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 0.999), color = "#1f77b4", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 1.09), color = "#2ca02c", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 0.873), color = "#d62728", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 1.26), color = "#ff7f0e", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 1.33), color = "#1f77b4", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 2.20), color = "#9467bd", size = 0.05, show.legend = FALSE) +
    geom_segment(aes(x = 2020, y = 1, xend = 2035, yend = 2.82), color = "#e377c2", size = 0.05, show.legend = FALSE) 
  
}

