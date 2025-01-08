
### Data harmonisation ###

Data_harmonisation <- function(data, name_object){

order_level <- levels(summary_monospe_EwE_1985$Group_name)
  
  new_data <- data %>%
    mutate(Group_name = case_when(
      Group_name == "Whale zooplankton eater" ~ "Whale zp eater",
      Group_name == "Seal hooded" ~ "Seal Hooded",
      Group_name == "Silver hake and pollock" ~ "Silver hake/ Saithe",
      Group_name == "Other plank-piscivorous fish" ~ "Other plank-pisc fish",
      Group_name == "American plaice greater than 35cm" ~ "AmPlaice > 35",
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
    mutate(Group_name = factor(Group_name, levels = order_level))
  
assign(name_object, new_data, envir = .GlobalEnv)

}
