

### All groups in ecopath database ###

group_name_key <- read.csv(here("EwE_database", "group_number_group_species_key.csv"))
EwE_number_group <- nrow(group_name_key)

# Observed Fishing mortality fixed value in ecopath 2023

F_Harp_seal_mortality <- 0.05510929 
F_Cod_mortality <- 0.01381579 
F_Capelin_mortality <- 0.009741046 

# Observed Biomass fixed value in ecopath 2023 (t/km^2)

Biomass_cod <- 0.76000 + 0.03806696 # Biomass cod sup 35 cm + Biomass cod inf 35 cm
Biomass_Capelin <- 4.97000 
Biomass_Harp_Seal <- 0.66500*0.4 # Biomass in habitat area * Hab area (proportion)
