

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

# Observed Fishing mortality fixed value in ecopath 2020 3LN0

F_Harp_seal_mortality <- 0.02684049 
F_Cod_mortality <- 0.0317992#9 
F_Capelin_mortality <- 0.0055428#57 

# Observed Biomass fixed value in ecopath 2020 3LN0 (t/km^2)

Biomass_cod <- 0.568 + 0.05214892 # Biomass cod sup 35 cm + Biomass cod inf 35 cm
Biomass_Capelin <- 3.5
Biomass_Harp_Seal <- 0.652*0.4 # Biomass in habitat area * Hab area (proportion)

size_area_3LNO <- 257400 # Km2
