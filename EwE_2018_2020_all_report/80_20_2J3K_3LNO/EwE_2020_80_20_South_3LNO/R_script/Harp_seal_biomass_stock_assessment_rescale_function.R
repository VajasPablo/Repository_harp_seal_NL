

Harp_seal_biomass_stock_assessment_rescale_function <- function(prop_sensitivity, region){
  
  size_area_2J3K <- 237600 # Km2
  size_area_3LNO <- 257400 # Km2
  biomass_2J3K <- 1.1310000420*0.400*size_area_2J3K
  biomass_3LNO <- 0.261*0.400*size_area_3LNO
  
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
  
  assign("heap_seal_biomass_stock_assessment", heap_seal_biomass_stock_assessment, envir = .GlobalEnv)
  
}
