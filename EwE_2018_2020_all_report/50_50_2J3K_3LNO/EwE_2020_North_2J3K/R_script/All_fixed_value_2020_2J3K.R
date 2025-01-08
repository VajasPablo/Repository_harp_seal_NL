

### All groups in ecopath database ###

group_name_key <- read.csv(here("EwE_database", "group_number_group_species_key.csv"))
EwE_number_group <- nrow(group_name_key)

### Standardisation of species name

group_name_key <- group_name_key %>% 
  mutate(Group_name = case_when(
    Group_name == "Seal harp" ~ "Seal Harp",
    Group_name == "Atlantic cod greater than 35cm" ~ "Cod> 35 cm",
    Group_name == "Atlantic cod less than 35cm" ~ "Cod<= 35 cm",
    TRUE ~ Group_name  
  ))

# Observed Fishing mortality fixed value in ecopath 2020 2J3K

F_Harp_seal_mortality <- 0.02828854 
F_Cod_mortality <- 0.0080917#43
F_Capelin_mortality <- 0.0037049#18 

# Observed Biomass fixed value in ecopath 2020 2J3K (t/km^2)

Biomass_cod <- 2.18 + 0.200149 # Biomass cod sup 35 cm + Biomass cod inf 35 cm
Biomass_Capelin <- 3.0499999523
Biomass_Harp_Seal <- 0.7070000172*0.4000000060 # Biomass in habitat area * Hab area (proportion)

size_area_2J3K <- 237600 # Km2
