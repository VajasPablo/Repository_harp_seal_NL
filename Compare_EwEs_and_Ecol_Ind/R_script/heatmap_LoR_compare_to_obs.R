
heatmap_LoR_compare_to_obs <- function() {
  

## Obs_1985

Obs_1985 <- Df_merged_seal_vs_cod_biomass_final_EwE_1985 %>% select(Group_name, biomass_annual_LoD_seal_obs_LoD_cod_obs) %>% 
  rename(obs_1985 = biomass_annual_LoD_seal_obs_LoD_cod_obs) %>% mutate(Group_name = factor(Group_name, levels = unique(Group_name)))

# LoR 100 cod Nord

Bcod_2J3K <- Bcod_simulation_biomass_EwE_2020_2J3K %>% select(Group_name, biomass_annual_Bcod_3.57600) %>% 
  rename(Cod_2J3K = biomass_annual_Bcod_3.57600) %>% filter(!Group_name %in% "Cod") %>% 
  mutate(Group_name = case_when(
    Group_name == "Whale zooplankton eater" ~ "Whale zp eater",
    Group_name == "Seal hooded" ~ "Seal Hooded",
    Group_name == "Silver hake and pollock" ~ "Silver hake/ Saithe",
    Group_name == "Other plank-piscivourous fish" ~ "Other plank-pisc fish",
    Group_name == "American plaice greater than 35cm" ~ "AmPlaice > 35 ",
    Group_name == "American plaice less than 35cm" ~ "AmPLaice <= 35",
    Group_name == "Other large benthivorous fish" ~ "Other L benthivorous fish",
    Group_name == "Other medium benthivourous fish" ~ "Other M benthivorous fish",
    Group_name == "Small benthivourous fish" ~ "Other S benthivorous fish",
    Group_name == "Large phytoplankton" ~ "Large Phytoplankton",
    Group_name == "Small phytoplankton" ~ "Small Phytoplankton",
    TRUE ~ Group_name)) %>%
  add_row(Group_name = "Whale Minke") %>%
  add_row(Group_name = "King crab") %>%
  add_row(Group_name = "Bacteria") %>%
  add_row(Group_name = "Heterotrophic nanoflagellates") %>%
  add_row(Group_name = "Silver hake/ Saithe") %>%
  add_row(Group_name = "Haddock") %>%
  add_row(Group_name = "Yellowtail flounder") %>% 
  mutate(Group_name = factor(Group_name, levels = Obs_1985$Group_name)) %>% 
  arrange(Group_name)

# LoR 100 cod Sud

Bcod_3LNO <- Bcod_simulation_biomass_EwE_2020_3LNO %>% select(Group_name, biomass_annual_Bcod_3.57600) %>% 
  rename(Cod_3LNO = biomass_annual_Bcod_3.57600) %>% filter(!Group_name %in% "Cod") %>% 
  mutate(Group_name = case_when(
    Group_name == "Whale zooplankton eater" ~ "Whale zp eater",
    Group_name == "Seal hooded" ~ "Seal Hooded",
    Group_name == "Silver hake and pollock" ~ "Silver hake/ Saithe",
    Group_name == "Other plank-piscivorous fish" ~ "Other plank-pisc fish",
    Group_name == "American plaice greater than 35cm" ~ "AmPlaice > 35 ",
    Group_name == "American plaice less than 35cm" ~ "AmPLaice <= 35",
    Group_name == "Other large benthivorous fish" ~ "Other L benthivorous fish",
    Group_name == "Other medium benthivorous fish" ~ "Other M benthivorous fish",
    Group_name == "Small benthivorous fish" ~ "Other S benthivorous fish",
    Group_name == "Large phytoplankton" ~ "Large Phytoplankton",
    Group_name == "Small phytoplankton" ~ "Small Phytoplankton",
    TRUE ~ Group_name)) %>%
  add_row(Group_name = "Whale Minke") %>%
  add_row(Group_name = "King crab") %>%
  add_row(Group_name = "Bacteria") %>%
  add_row(Group_name = "Heterotrophic nanoflagellates") %>%
  mutate(Group_name = factor(Group_name, levels = Obs_1985$Group_name)) %>% 
  arrange(Group_name)

# LoR 100 capelin Nord

Bcapelin_2J3K <- Bcapelin_simulation_biomass_EwE_2020_2J3K %>% select(Group_name, biomass_annual_Bcapelin_13.77000) %>% 
  rename(Capelin_2J3K = biomass_annual_Bcapelin_13.77000) %>% 
  mutate(Group_name = case_when(
    Group_name == "Whale zooplankton eater" ~ "Whale zp eater",
    Group_name == "Seal hooded" ~ "Seal Hooded",
    Group_name == "Silver hake and pollock" ~ "Silver hake/ Saithe",
    Group_name == "Other plank-piscivourous fish" ~ "Other plank-pisc fish",
    Group_name == "American plaice greater than 35cm" ~ "AmPlaice > 35 ",
    Group_name == "American plaice less than 35cm" ~ "AmPLaice <= 35",
    Group_name == "Other large benthivorous fish" ~ "Other L benthivorous fish",
    Group_name == "Other medium benthivourous fish" ~ "Other M benthivorous fish",
    Group_name == "Small benthivourous fish" ~ "Other S benthivorous fish",
    Group_name == "Large phytoplankton" ~ "Large Phytoplankton",
    Group_name == "Small phytoplankton" ~ "Small Phytoplankton",
    TRUE ~ Group_name)) %>%
  add_row(Group_name = "Whale Minke") %>%
  add_row(Group_name = "King crab") %>%
  add_row(Group_name = "Bacteria") %>%
  add_row(Group_name = "Heterotrophic nanoflagellates") %>%
  add_row(Group_name = "Silver hake/ Saithe") %>%
  add_row(Group_name = "Haddock") %>%
  add_row(Group_name = "Yellowtail flounder") %>% 
  mutate(Group_name = factor(Group_name, levels = Obs_1985$Group_name)) %>% 
  arrange(Group_name)

# LoR 100 capelin Sud 

Bcapelin_3LNO <- Bcapelin_simulation_biomass_EwE_2020_3LNO %>% select(Group_name, biomass_annual_Bcapelin_13.7700) %>% 
  rename(Capelin_3LNO = biomass_annual_Bcapelin_13.7700) %>% 
  mutate(Group_name = case_when(
    Group_name == "Whale zooplankton eater" ~ "Whale zp eater",
    Group_name == "Seal hooded" ~ "Seal Hooded",
    Group_name == "Silver hake and pollock" ~ "Silver hake/ Saithe",
    Group_name == "Other plank-piscivorous fish" ~ "Other plank-pisc fish",
    Group_name == "American plaice greater than 35cm" ~ "AmPlaice > 35 ",
    Group_name == "American plaice less than 35cm" ~ "AmPLaice <= 35",
    Group_name == "Other large benthivorous fish" ~ "Other L benthivorous fish",
    Group_name == "Other medium benthivorous fish" ~ "Other M benthivorous fish",
    Group_name == "Small benthivorous fish" ~ "Other S benthivorous fish",
    Group_name == "Large phytoplankton" ~ "Large Phytoplankton",
    Group_name == "Small phytoplankton" ~ "Small Phytoplankton",
    TRUE ~ Group_name)) %>%
  add_row(Group_name = "Whale Minke") %>%
  add_row(Group_name = "King crab") %>%
  add_row(Group_name = "Bacteria") %>%
  add_row(Group_name = "Heterotrophic nanoflagellates") %>%
  mutate(Group_name = factor(Group_name, levels = Obs_1985$Group_name)) %>% 
  arrange(Group_name)

# LoR 100x100 Nord 

Bcod_Bcapelin_2J3K <- Bcod_Bcapelin_simulation_biomass_EwE_2020_2J3K %>% select(Group_name, biomass_annual_LoR_cod_100_LoR_capelin_100) %>% 
  rename(Bcod_Bcapelin_2J3K = biomass_annual_LoR_cod_100_LoR_capelin_100) %>% 
  mutate(Group_name = case_when(
    Group_name == "Whale zooplankton eater" ~ "Whale zp eater",
    Group_name == "Seal hooded" ~ "Seal Hooded",
    Group_name == "Silver hake and pollock" ~ "Silver hake/ Saithe",
    Group_name == "Other plank-piscivourous fish" ~ "Other plank-pisc fish",
    Group_name == "American plaice greater than 35cm" ~ "AmPlaice > 35 ",
    Group_name == "American plaice less than 35cm" ~ "AmPLaice <= 35",
    Group_name == "Other large benthivorous fish" ~ "Other L benthivorous fish",
    Group_name == "Other medium benthivourous fish" ~ "Other M benthivorous fish",
    Group_name == "Small benthivourous fish" ~ "Other S benthivorous fish",
    Group_name == "Large phytoplankton" ~ "Large Phytoplankton",
    Group_name == "Small phytoplankton" ~ "Small Phytoplankton",
    TRUE ~ Group_name)) %>%
  add_row(Group_name = "Whale Minke") %>%
  add_row(Group_name = "King crab") %>%
  add_row(Group_name = "Bacteria") %>%
  add_row(Group_name = "Heterotrophic nanoflagellates") %>%
  add_row(Group_name = "Silver hake/ Saithe") %>%
  add_row(Group_name = "Haddock") %>%
  add_row(Group_name = "Yellowtail flounder") %>% 
  mutate(Group_name = factor(Group_name, levels = Obs_1985$Group_name)) %>% 
  arrange(Group_name)


# LoR 100x100 Sud  

Bcod_Bcapelin_3LNO <- Bcod_Bcapelin_simulation_biomass_EwE_2020_3LNO %>% select(Group_name, biomass_annual_LoR_cod_100_LoR_capelin_100) %>% 
  rename(Bcod_Bcapelin_3LNO = biomass_annual_LoR_cod_100_LoR_capelin_100) %>% 
  mutate(Group_name = case_when(
    Group_name == "Whale zooplankton eater" ~ "Whale zp eater",
    Group_name == "Seal hooded" ~ "Seal Hooded",
    Group_name == "Silver hake and pollock" ~ "Silver hake/ Saithe",
    Group_name == "Other plank-piscivorous fish" ~ "Other plank-pisc fish",
    Group_name == "American plaice greater than 35cm" ~ "AmPlaice > 35 ",
    Group_name == "American plaice less than 35cm" ~ "AmPLaice <= 35",
    Group_name == "Other large benthivorous fish" ~ "Other L benthivorous fish",
    Group_name == "Other medium benthivorous fish" ~ "Other M benthivorous fish",
    Group_name == "Small benthivorous fish" ~ "Other S benthivorous fish",
    Group_name == "Large phytoplankton" ~ "Large Phytoplankton",
    Group_name == "Small phytoplankton" ~ "Small Phytoplankton",
    TRUE ~ Group_name)) %>%
  add_row(Group_name = "Whale Minke") %>%
  add_row(Group_name = "King crab") %>%
  add_row(Group_name = "Bacteria") %>%
  add_row(Group_name = "Heterotrophic nanoflagellates") %>%
  mutate(Group_name = factor(Group_name, levels = Obs_1985$Group_name)) %>% 
  arrange(Group_name)

Obs_1985
Bcod_2J3K
Bcod_3LNO
Bcapelin_2J3K
Bcod_Bcapelin_2J3K
Bcod_Bcapelin_3LNO

merged_table <- cbind.data.frame(Obs_1985, 
                         Bcod_2J3K$Cod_2J3K, 
                         Bcod_3LNO$Cod_3LNO, 
                         Bcapelin_2J3K$Capelin_2J3K, 
                         Bcapelin_3LNO$Capelin_3LNO,
                         Bcod_Bcapelin_2J3K$Bcod_Bcapelin_2J3K, 
                         Bcod_Bcapelin_3LNO$Bcod_Bcapelin_3LNO) %>% na.omit() %>% rename(`cod 2J3K` = "Bcod_2J3K$Cod_2J3K", 
                                                                                         `cod 3LNO` = "Bcod_3LNO$Cod_3LNO", 
                                                                                         `capelin 2J3K` = "Bcapelin_2J3K$Capelin_2J3K", 
                                                                                         `capelin 3LNO` = "Bcapelin_3LNO$Capelin_3LNO",
                                                                                         `cod capelin 2J3K` = "Bcod_Bcapelin_2J3K$Bcod_Bcapelin_2J3K", 
                                                                                         `cod capelin 3LNO` = "Bcod_Bcapelin_3LNO$Bcod_Bcapelin_3LNO",
                                                                                        `Obs 1985` = "obs_1985")

merged_table <- merged_table %>%
  mutate(across(-Group_name, as.numeric))

percentage_change <- merged_table %>%
  mutate(across(-Group_name, ~ 100 * (. - `Obs 1985`) / `Obs 1985`)) %>%
  mutate(Group_name = as.character(Group_name),
         Group_name = factor(Group_name, levels = Group_name))

heatmap_data <- percentage_change %>%
  pivot_longer(cols = -Group_name, names_to = "Simulation", values_to = "Value") %>%
  mutate(Simulation = factor(Simulation, levels = c("Obs 1985", "cod 2J3K", "cod 3LNO", "capelin 2J3K", "capelin 3LNO", "cod capelin 2J3K", "cod capelin 3LNO"))) 

breaks <- c(-100, -50, 0, 500, 1000)
colors <- c("#f28482", "#f5cac3", "white", "#b0c4b1", "#4a5759")

gg_heatmap_data <- ggplot(heatmap_data, aes(x = Simulation, y = Group_name, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = colors,
    values = scales::rescale(c(-100, -50, 0, 500, 1000)),  # Normaliser les valeurs de coupure
    breaks = breaks,
    labels = c("-100", "-50", "0", "500", "1000"),
    name = "Change (%)"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 9),
        strip.text = element_text(size = 14)) +
  coord_fixed() +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(heatmap_data$Group_name))) + 
  labs(x = "100% Level of Restoration Simulations", y = "EwE Commun Species Group")

print(gg_heatmap_data)

}

