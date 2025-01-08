
heatmap_multispecific_biomass_LoR_function_2J3K <- function(data, spe_title){
  
  data_process <- data %>%
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
    mutate(is_positive = if_else(seal_biomass_change >= 0, "positive", "non-positive"))
  

  ggplot_biomass_change <- ggplot(data_process, aes(x = LoR_cod, y = LoR_capelin, fill = seal_biomass_change)) +
    geom_tile(aes(color = is_positive), size = 1.5) +
    scale_color_manual(values = c("positive" = "red", "non-positive" = "transparent"), guide = "none") +
    scale_fill_gradientn(colours = c("#d9ed92", "#b5e48c", "#99d98c", "#76c893", "#52b69a", "#34a0a4"),
                         limits = range(data_process$seal_biomass_change, na.rm = TRUE),
                         breaks = pretty_breaks(n = 6)) +
    geom_text(aes(label = paste0(round(seal_biomass_change, 2), "%")), color = "black", size = 4) +
    theme_minimal() +
    scale_x_discrete(position = "top") +
    labs(title = spe_title,
         x = "Level of Restoration Cod",
         y = "Level of Restoration Capelin",
         fill = "Harp seal\ngrowth (%)") +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12))
  
}
