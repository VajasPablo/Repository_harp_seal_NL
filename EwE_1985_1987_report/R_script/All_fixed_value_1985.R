

### All groups in ecopath database ###

group_name_key <- read.csv(here("EwE_database", "group_number_group_species_key.csv"))
EwE_number_group <- nrow(group_name_key)

# Observed Fishing mortality fixed value in ecopath 1985

F_Harp_seal_mortality <- 0.0144 
F_Cod_mortality <- 0.169 
F_Capelin_mortality <- 0.00915 

# Observed Biomass fixed value in ecopath 1985 (t/km^2)

Biomass_cod <- 3.576 + 0.958 # Biomass cod sup 35 cm + Biomass cod inf 35 cm
Biomass_Capelin <- 13.77 
Biomass_Harp_Seal <- 0.534*0.4 # Biomass in habitat area * Hab area (proportion)
