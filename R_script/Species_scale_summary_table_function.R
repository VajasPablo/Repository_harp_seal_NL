
# Species scale summary table 

Species_scale_summary_table_function <- function(){

Total_table_col_name <- c("EwE_Model", "Species", "TL", "Connectance", "SURF", "OI", "Key_1", "Key_2", "RTI", "O_C", "Relative_Biomass")

seal_1985 <- c("EwE 1985", "Harp seal", 
               `EwE_1985_Trophic_Level_Seal Harp`, 
               `EwE_1985_Connectance_Index_Seal Harp`,
               `EwE_1985_SURF_Index_Seal Harp`,
               `EwE_1985_Omnivory_index_Seal Harp`,
               `EwE_1985_Keystone_RTI_Seal Harp`$Keystone_index_1, 
               `EwE_1985_Keystone_RTI_Seal Harp`$Keystone_index_2, 
               `EwE_1985_Keystone_RTI_Seal Harp`$Relative_total_impact, 
               `EwE_1985_Production_consumption_Seal Harp`,
               EwE_1985_harp_seal_relative_biomass)

codsup35_1985 <- c("EwE 1985", "Cod> 35 cm",
                   `EwE_1985_Trophic_Level_Cod> 35 cm` , 
                   `EwE_1985_Connectance_Index_Cod> 35 cm`,
                   `EwE_1985_SURF_Index_Cod> 35 cm`,
                   `EwE_1985_Omnivory_index_Cod> 35 cm`,
                   `EwE_1985_Keystone_RTI_Cod> 35 cm`$Keystone_index_1, 
                   `EwE_1985_Keystone_RTI_Cod> 35 cm`$Keystone_index_2, 
                   `EwE_1985_Keystone_RTI_Cod> 35 cm`$Relative_total_impact, 
                   `EwE_1985_Production_consumption_Cod> 35 cm`,
                   EwE_1985_cod_sup_35_relative_biomass)

codinf35_1985 <- c("EwE 1985", "Cod<= 35 cm",
                   `EwE_1985_Trophic_Level_Cod<= 35 cm` , 
                   `EwE_1985_Connectance_Index_Cod<= 35 cm`, 
                   `EwE_1985_SURF_Index_Cod<= 35 cm`,
                   `EwE_1985_Omnivory_index_Cod<= 35 cm`,
                   `EwE_1985_Keystone_RTI_Cod<= 35 cm`$Keystone_index_1, 
                   `EwE_1985_Keystone_RTI_Cod<= 35 cm`$Keystone_index_2, 
                   `EwE_1985_Keystone_RTI_Cod<= 35 cm`$Relative_total_impact, 
                   `EwE_1985_Production_consumption_Cod<= 35 cm`,
                   EwE_1985_cod_inf_35_relative_biomass)

capelin_1985 <- c("EwE 1985", "Capelin",
                  EwE_1985_Trophic_Level_Capelin, 
                  EwE_1985_Connectance_Index_Capelin,
                  EwE_1985_SURF_Index_Capelin,
                  EwE_1985_Omnivory_index_Capelin,
                  EwE_1985_Keystone_RTI_Capelin$Keystone_index_1, 
                  EwE_1985_Keystone_RTI_Capelin$Keystone_index_2, 
                  EwE_1985_Keystone_RTI_Capelin$Relative_total_impact, 
                  EwE_1985_Production_consumption_Capelin,
                  EwE_1985_capelin_relative_biomass)

seal_2013 <- c("EwE 2013", "Harp seal", 
               `EwE_2013_Trophic_Level_Seal Harp`, 
               `EwE_2013_Connectance_Index_Seal Harp`,
               `EwE_2013_SURF_Index_Seal Harp`,
               `EwE_2013_Omnivory_index_Seal Harp`,
               `EwE_2013_Keystone_RTI_Seal Harp`$Keystone_index_1, 
               `EwE_2013_Keystone_RTI_Seal Harp`$Keystone_index_2, 
               `EwE_2013_Keystone_RTI_Seal Harp`$Relative_total_impact, 
               `EwE_2013_Production_consumption_Seal Harp`,
               EwE_2013_harp_seal_relative_biomass)

codsup35_2013 <- c("EwE 2013", "Cod> 35 cm",
                   `EwE_2013_Trophic_Level_Cod> 35 cm` , 
                   `EwE_2013_Connectance_Index_Cod> 35 cm`,
                   `EwE_2013_SURF_Index_Cod> 35 cm`,
                   `EwE_2013_Omnivory_index_Cod> 35 cm`,
                   `EwE_2013_Keystone_RTI_Cod> 35 cm`$Keystone_index_1, 
                   `EwE_2013_Keystone_RTI_Cod> 35 cm`$Keystone_index_2, 
                   `EwE_2013_Keystone_RTI_Cod> 35 cm`$Relative_total_impact, 
                   `EwE_2013_Production_consumption_Cod> 35 cm`,
                   EwE_2013_cod_sup_35_relative_biomass)

codinf35_2013 <- c("EwE 2013", "Cod<= 35 cm",
                   `EwE_2013_Trophic_Level_Cod<= 35 cm` , 
                   `EwE_2013_Connectance_Index_Cod<= 35 cm`, 
                   `EwE_2013_SURF_Index_Cod<= 35 cm`,
                   `EwE_2013_Omnivory_index_Cod<= 35 cm`,
                   `EwE_2013_Keystone_RTI_Cod<= 35 cm`$Keystone_index_1, 
                   `EwE_2013_Keystone_RTI_Cod<= 35 cm`$Keystone_index_2, 
                   `EwE_2013_Keystone_RTI_Cod<= 35 cm`$Relative_total_impact, 
                   `EwE_2013_Production_consumption_Cod<= 35 cm`,
                   EwE_2013_cod_inf_35_relative_biomass)

capelin_2013 <- c("EwE 2013", "Capelin",
                  EwE_2013_Trophic_Level_Capelin, 
                  EwE_2013_Connectance_Index_Capelin,
                  EwE_2013_SURF_Index_Capelin,
                  EwE_2013_Omnivory_index_Capelin,
                  EwE_2013_Keystone_RTI_Capelin$Keystone_index_1, 
                  EwE_2013_Keystone_RTI_Capelin$Keystone_index_2, 
                  EwE_2013_Keystone_RTI_Capelin$Relative_total_impact, 
                  EwE_2013_Production_consumption_Capelin,
                  EwE_2013_capelin_relative_biomass)

seal_2020_2J3K <- c("EwE 2020 2J3K", "Harp seal", 
                    `EwE_2020_2J3K_Trophic_Level_Seal Harp`, 
                    `EwE_2020_2J3K_Connectance_Index_Seal Harp`,
                    `EwE_2020_2J3K_SURF_Index_Seal Harp`,
                    `EwE_2020_2J3K_Omnivory_index_Seal Harp`,
                    `EwE_2020_2J3K_Keystone_RTI_Seal Harp`$Keystone_index_1, 
                    `EwE_2020_2J3K_Keystone_RTI_Seal Harp`$Keystone_index_2, 
                    `EwE_2020_2J3K_Keystone_RTI_Seal Harp`$Relative_total_impact, 
                    `EwE_2020_2J3K_Production_consumption_Seal Harp`,
                    EwE_2020_2J3K_harp_seal_relative_biomass)

codsup35_2020_2J3K <- c("EwE 2020 2J3K", "Cod> 35 cm",
                        `EwE_2020_2J3K_Trophic_Level_Cod> 35 cm` , 
                        `EwE_2020_2J3K_Connectance_Index_Cod> 35 cm`,
                        `EwE_2020_2J3K_SURF_Index_Cod> 35 cm`,
                        `EwE_2020_2J3K_Omnivory_index_Cod> 35 cm`,
                        `EwE_2020_2J3K_Keystone_RTI_Cod> 35 cm`$Keystone_index_1, 
                        `EwE_2020_2J3K_Keystone_RTI_Cod> 35 cm`$Keystone_index_2, 
                        `EwE_2020_2J3K_Keystone_RTI_Cod> 35 cm`$Relative_total_impact, 
                        `EwE_2020_2J3K_Production_consumption_Cod> 35 cm`,
                        EwE_2020_2J3K_cod_sup_35_relative_biomass)

codinf35_2020_2J3K <- c("EwE 2020 2J3K", "Cod<= 35 cm",
                        `EwE_2020_2J3K_Trophic_Level_Cod<= 35 cm` , 
                        `EwE_2020_2J3K_Connectance_Index_Cod<= 35 cm`, 
                        `EwE_2020_2J3K_SURF_Index_Cod<= 35 cm`,
                        `EwE_2020_2J3K_Omnivory_index_Cod<= 35 cm`,
                        `EwE_2020_2J3K_Keystone_RTI_Cod<= 35 cm`$Keystone_index_1, 
                        `EwE_2020_2J3K_Keystone_RTI_Cod<= 35 cm`$Keystone_index_2, 
                        `EwE_2020_2J3K_Keystone_RTI_Cod<= 35 cm`$Relative_total_impact, 
                        `EwE_2020_2J3K_Production_consumption_Cod<= 35 cm`,
                        EwE_2020_2J3K_cod_inf_35_relative_biomass)

capelin_2020_2J3K <- c("EwE 2020 2J3K", "Capelin",
                       EwE_2020_2J3K_Trophic_Level_Capelin, 
                       EwE_2020_2J3K_Connectance_Index_Capelin,
                       EwE_2020_2J3K_SURF_Index_Capelin,
                       EwE_2020_2J3K_Omnivory_index_Capelin,
                       EwE_2020_2J3K_Keystone_RTI_Capelin$Keystone_index_1, 
                       EwE_2020_2J3K_Keystone_RTI_Capelin$Keystone_index_2, 
                       EwE_2020_2J3K_Keystone_RTI_Capelin$Relative_total_impact, 
                       EwE_2020_2J3K_Production_consumption_Capelin,
                       EwE_2020_2J3K_capelin_relative_biomass)

seal_2020_3LNO <- c("EwE 2020 3LNO", "Harp seal", 
                    `EwE_2020_3LNO_Trophic_Level_Seal Harp`, 
                    `EwE_2020_3LNO_Connectance_Index_Seal Harp`,
                    `EwE_2020_3LNO_SURF_Index_Seal Harp`,
                    `EwE_2020_3LNO_Omnivory_index_Seal Harp`,
                    `EwE_2020_3LNO_Keystone_RTI_Seal Harp`$Keystone_index_1, 
                    `EwE_2020_3LNO_Keystone_RTI_Seal Harp`$Keystone_index_2, 
                    `EwE_2020_3LNO_Keystone_RTI_Seal Harp`$Relative_total_impact, 
                    `EwE_2020_3LNO_Production_consumption_Seal Harp`,
                    EwE_2020_3LNO_harp_seal_relative_biomass)

codsup35_2020_3LNO <- c("EwE 2020 3LNO", "Cod> 35 cm",
                        `EwE_2020_3LNO_Trophic_Level_Cod> 35 cm` , 
                        `EwE_2020_3LNO_Connectance_Index_Cod> 35 cm`,
                        `EwE_2020_3LNO_SURF_Index_Cod> 35 cm`,
                        `EwE_2020_3LNO_Omnivory_index_Cod> 35 cm`,
                        `EwE_2020_3LNO_Keystone_RTI_Cod> 35 cm`$Keystone_index_1, 
                        `EwE_2020_3LNO_Keystone_RTI_Cod> 35 cm`$Keystone_index_2, 
                        `EwE_2020_3LNO_Keystone_RTI_Cod> 35 cm`$Relative_total_impact, 
                        `EwE_2020_3LNO_Production_consumption_Cod> 35 cm`,
                        EwE_2020_3LNO_cod_sup_35_relative_biomass)

codinf35_2020_3LNO <- c("EwE 2020 3LNO", "Cod<= 35 cm",
                        `EwE_2020_3LNO_Trophic_Level_Cod<= 35 cm` , 
                        `EwE_2020_3LNO_Connectance_Index_Cod<= 35 cm`, 
                        `EwE_2020_3LNO_SURF_Index_Cod<= 35 cm`,
                        `EwE_2020_3LNO_Omnivory_index_Cod<= 35 cm`,
                        `EwE_2020_3LNO_Keystone_RTI_Cod<= 35 cm`$Keystone_index_1, 
                        `EwE_2020_3LNO_Keystone_RTI_Cod<= 35 cm`$Keystone_index_2, 
                        `EwE_2020_3LNO_Keystone_RTI_Cod<= 35 cm`$Relative_total_impact, 
                        `EwE_2020_3LNO_Production_consumption_Cod<= 35 cm`,
                        EwE_2020_3LNO_cod_inf_35_relative_biomass)

capelin_2020_3LNO <- c("EwE 2020 3LNO", "Capelin",
                       EwE_2020_3LNO_Trophic_Level_Capelin, 
                       EwE_2020_3LNO_Connectance_Index_Capelin,
                       EwE_2020_3LNO_SURF_Index_Capelin,
                       EwE_2020_3LNO_Omnivory_index_Capelin,
                       EwE_2020_3LNO_Keystone_RTI_Capelin$Keystone_index_1, 
                       EwE_2020_3LNO_Keystone_RTI_Capelin$Keystone_index_2, 
                       EwE_2020_3LNO_Keystone_RTI_Capelin$Relative_total_impact, 
                       EwE_2020_3LNO_Production_consumption_Capelin,
                       EwE_2020_3LNO_capelin_relative_biomass)

cod_all_1985 <- c("EwE 1985", "Cod all",
                  NA, 
                  EwE_1985_Connectance_Index_Cod,
                  NA,
                  NA,
                  NA, 
                  NA, 
                  NA, 
                  NA,
                  EwE_1985_cod_all_relative_biomass)

cod_all_2013 <- c("EwE 2013", "Cod all",
                  NA, 
                  EwE_2013_Connectance_Index_Cod,
                  NA,
                  NA,
                  NA, 
                  NA, 
                  NA, 
                  NA,
                  EwE_2013_cod_all_relative_biomass)

cod_all_2020_2J3K <- c("EwE 2020 2J3K", "Cod all",
                       NA, 
                       EwE_2020_2J3K_Connectance_Index_Cod,
                       NA,
                       NA,
                       NA, 
                       NA, 
                       NA, 
                       NA,
                       EwE_2020_2J3K_cod_all_relative_biomass)

cod_all_2020_3LNO <- c("EwE 2020 3LNO", "Cod all",
                       NA, 
                       EwE_2020_3LNO_Connectance_Index_Cod,
                       NA,
                       NA,
                       NA, 
                       NA, 
                       NA, 
                       NA,
                       EwE_2020_3LNO_cod_all_relative_biomass)

`Species_scale_summary_table` <- rbind.data.frame(seal_1985, cod_all_1985, codsup35_1985, codinf35_1985, capelin_1985,
                                                seal_2013, cod_all_2013, codsup35_2013, codinf35_2013, capelin_2013,
                                                seal_2020_2J3K, cod_all_2020_2J3K, codsup35_2020_2J3K, codinf35_2020_2J3K, capelin_2020_2J3K,
                                                seal_2020_3LNO, cod_all_2020_3LNO, codsup35_2020_3LNO, codinf35_2020_3LNO, capelin_2020_3LNO) %>% 
  rename_with(~ Total_table_col_name, everything()) %>%
  mutate_at(vars(TL, Connectance, SURF, OI, Key_1, Key_2, RTI, O_C, Relative_Biomass), as.numeric) %>%
  mutate(
    TL = round(TL, 4),
    Connectance = round(Connectance, 4),
    SURF = round(SURF, 6),
    OI = round(OI, 4),
    Key_1 = round(Key_1, 4),
    Key_2 = round(Key_2, 4),
    RTI = round(RTI, 4),
    O_C = round(O_C, 4),
    Relative_Biomass = round(Relative_Biomass, 4)
  )

assign("Species_scale_summary_table", Species_scale_summary_table, envir = .GlobalEnv)

datatable(data = Species_scale_summary_table, filter="top", 
          rownames = FALSE, options = list(pageLength = 20, scrollX=T))

}
