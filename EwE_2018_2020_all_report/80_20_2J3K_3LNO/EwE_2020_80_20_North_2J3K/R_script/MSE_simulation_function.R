
Harp_seal_MSE_simulation_function_2J3K <- function(data_simulation, data_assessment){

#size_area_2J3K <- 237600 # Km2
# 1985 : (0.4*0.534*(237600+257400))*(237600)/(257400+237600)
# 2013 : (0.4*0.665*(237600+257400))*(237600)/(257400+237600)
#size_area_3LNO <- 257400 # Km2
  
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
  mutate(Harp_seal_total_biomass_2J3K = biomass_seal*237600) %>%
  mutate(Year = 2035) %>%   mutate(
    LoR_cod = str_replace_all(LoR_cod, "_", " "),
    LoR_capelin = str_replace_all(LoR_capelin, "_", " ")
  ) %>%
  mutate(
    LoR_cod = factor(LoR_cod, levels = c("LoR Cod 1", "LoR Cod 25", "LoR Cod 50", "LoR Cod obs", "LoR Cod 75", "LoR Cod 100")),
    LoR_capelin = factor(LoR_capelin, levels = c("LoR Capelin 1", "LoR Capelin obs", "LoR Capelin 25", "LoR Capelin 50", "LoR Capelin 75", "LoR Capelin 100"))
  )

gg <- ggplot(data_assessment, aes(x = Year, y = Biomass_T_area_rescale)) +
  geom_line() + 
  labs(x = "Year", y = "Biomass_T_area_rescale")+
  geom_point(data = data_process[data_process$Year == 2035, ], 
             aes(x = Year, y = Harp_seal_total_biomass_2J3K, color = LoR_cod, shape = LoR_capelin),  
             size = 4, show.legend = TRUE) +  
  scale_color_manual(values = c("LoR Cod obs" = "blue", "LoR Cod 25" = "green", "LoR Cod 1" = "red", "LoR Cod 50" = "orange", "LoR Cod 75" = "purple", "LoR Cod 100" = "pink")) +
  scale_shape_manual(values = c("LoR Capelin obs" = 0, "LoR Capelin 25" = 1, "LoR Capelin 1" = 2, "LoR Capelin 50" = 3, "LoR Capelin 75" = 4, "LoR Capelin 100" = 5)) +
  #geom_vline(xintercept = c(1985, 2013, 2020), linetype = "dotted", color = "yellow4") +
  geom_hline(yintercept = 107490.24, linetype = "dashed", color = "black") +
  geom_hline(yintercept = max(data_assessment$Biomass_T_area_rescale)*1.6, linetype = "dashed", color = "yellow4") +
 # annotate(geom = "text", x = c(1950), y = max(data_assessment$Biomass_T_area_rescale)*1.6, 
 #       label = c("\u2190 Historical population \nlevel in 1800'"), vjust = 2, hjust = 0, angle = 0, size = 4, color = "darkred") +
 # annotate(geom = "text", x = c(1985, 2013, 2020), y = min(data_assessment$Biomass_T_area_rescale), 
 #         label = c("EwE 1985", "EwE 2013", "EwE 2020 2J3K"), vjust = 0.1, hjust = -0.3, angle = 90, size = 4, color = "yellow4") +
  #geom_point(x = c(1985), y = c(50751.36), color = "gray30",size = 2)  + #52866
  #geom_point(x = c(2013), y = c(63201.6), color = "gray30",size = 2)  + #65835
  #geom_point(x = c(2020), y = c(107490.24), color = "gray30",size = 2)  +
 # annotate("text", x = c(1985, 2013), y = c(52866, 65835), label = "EwE \nvalue", vjust = -0.5, size = 3, color = "black") +
  geom_segment(aes(x = 2020, y = 107490.24, xend = 2035, yend = 107498), color = "blue", size = 0.05, show.legend = FALSE) +
  geom_segment(aes(x = 2020, y = 107490.24, xend = 2035, yend = 102971), color = "green", size = 0.05, show.legend = FALSE) +
  geom_segment(aes(x = 2020, y = 107490.24, xend = 2035, yend = 104364), color = "red", size = 0.05, show.legend = FALSE) +
  geom_segment(aes(x = 2020, y = 107490.24, xend = 2035, yend = 105323), color = "orange", size = 0.05, show.legend = FALSE) +
  geom_segment(aes(x = 2020, y = 107490.24, xend = 2035, yend = 120253), color = "blue", size = 0.05, show.legend = FALSE) +
  geom_segment(aes(x = 2020, y = 107490.24, xend = 2035, yend = 137491), color = "purple", size = 0.05, show.legend = FALSE) +
  geom_segment(aes(x = 2020, y = 107490.24, xend = 2035, yend = 158602), color = "pink", size = 0.05, show.legend = FALSE) +
  scale_y_continuous(
    limits = c(0, 300000),                   
    breaks = seq(0, 300000, by = 50000),
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
        legend.text = element_text(size = 10)) + ylab("Rescale Harp seal Biomass (t)")

print(gg)

}

