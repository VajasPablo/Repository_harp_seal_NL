
# Ecosystem scale summary table 

Ecosystem_scale_summary_table_function <- function(){ 

Total_table_col_name <- c("EwE_Model", "SOI", "Connectance", "O_C", "TE")

Total_table_1985 <- c("EwE 1985", EwE_1985_SOI , EwE_1985_connectance, EwE_1985_OC, EwE_1985_network_analysis_transfer_efficiency[1,2])
Total_table_2013 <- c("EwE 2013", EwE_2013_SOI , EwE_2013_connectance, EwE_2013_OC, EwE_2013_network_analysis_transfer_efficiency[1,2])
Total_table_2020_2J3K <- c("EwE 2020 2J3K", EwE_2020_2J3K_SOI , EwE_2020_2J3K_connectance, EwE_2020_2J3K_OC, EwE_2020_2J3K_network_analysis_transfer_efficiency[1,2])
Total_table_2020_3LNO <- c("EwE 2020 3LNO", EwE_2020_3LNO_SOI , EwE_2020_3LNO_connectance, EwE_2020_3LNO_OC, EwE_2020_3LNO_network_analysis_transfer_efficiency[1,2])

Ecosystem_scale_summary_table <- rbind.data.frame(Total_table_1985, Total_table_2013, Total_table_2020_2J3K, Total_table_2020_3LNO) %>% 
  rename_with(~ Total_table_col_name, everything())

Ecosystem_scale_summary_table$Total_biomass <- c(EwE_1985_total_biomass, EwE_2013_total_biomass, EwE_2020_2J3K_total_biomass, EwE_2020_3LNO_total_biomass)

assign("Ecosystem_scale_summary_table", Ecosystem_scale_summary_table, envir = .GlobalEnv)

datatable(data = Ecosystem_scale_summary_table, filter="top", 
          rownames = FALSE, options = list(pageLength = 20, scrollX=T))
}

