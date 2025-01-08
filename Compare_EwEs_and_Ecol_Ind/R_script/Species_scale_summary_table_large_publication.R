
Species_scale_summary_table_large_publication <- function(){

Species_scale_summary_table_large <<- Species_scale_summary_table_large %>% 
  rename(`Periods-Areas` = EwE_Model) %>% 
  mutate(
    `Periods-Areas` = case_when(
      `Periods-Areas` == "EwE 1985" ~ "1985-1987",
      `Periods-Areas` == "EwE 2013" ~ "2013-2015",
      `Periods-Areas` == "EwE 2020 2J3K" ~ "2018-2020 2J3K",
      `Periods-Areas` == "EwE 2020 3LNO" ~ "2018-2020 3LNO",
      TRUE ~ `Periods-Areas`
    )
  ) %>%
  mutate(
    Species = case_when(
      Species == "Cod> 35 cm" ~ "Cod > 35cm",
      Species == "Cod<= 35 cm" ~ "Cod ≤ 35cm",
      TRUE ~ Species
    )
  )

}