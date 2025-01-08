


compare_heatmap_monospe <- function(data_period_1, period_1, data_period_2, period_2){
  
  df_1 <- data_period_1 %>%
    rename(!!paste0("Value_", period_1) := Value) %>%
    mutate(!!paste0("Value_", period_1) := case_when(
      get(paste0("Value_", period_1)) == "0" ~ "0",
      get(paste0("Value_", period_1)) == "-" ~ "-1",
      get(paste0("Value_", period_1)) == "- -" ~ "-2",
      get(paste0("Value_", period_1)) == "- - -" ~ "-3",
      get(paste0("Value_", period_1)) == "+" ~ "1",
      get(paste0("Value_", period_1)) == "+ +" ~ "2",
      get(paste0("Value_", period_1)) == "+ + +" ~ "3",
      TRUE ~ as.character(!!paste0("Value_", period_1))
    ),
    !!paste0("Value_", period_1) := as.numeric(get(!!paste0("Value_", period_1))))
  
  df_2 <- data_period_2 %>%
    rename(!!paste0("Value_", period_2) := Value) %>%
    mutate(!!paste0("Value_", period_2) := case_when(
      get(paste0("Value_", period_2)) == "0" ~ "0",
      get(paste0("Value_", period_2)) == "-" ~ "-1",
      get(paste0("Value_", period_2)) == "- -" ~ "-2",
      get(paste0("Value_", period_2)) == "- - -" ~ "-3",
      get(paste0("Value_", period_2)) == "+" ~ "1",
      get(paste0("Value_", period_2)) == "+ +" ~ "2",
      get(paste0("Value_", period_2)) == "+ + +" ~ "3",
      TRUE ~ as.character(!!paste0("Value_", period_2))
    ),
    !!paste0("Value_", period_2) := as.numeric(get(!!paste0("Value_", period_2))))
  
  df_merged <- left_join(df_1, df_2) %>% mutate(Change = get(paste0("Value_", period_2)) - get(paste0("Value_", period_1))) %>%
    mutate(Change = case_when(
      Change == "0" ~ "0",
      Change == -1 ~ "-",
      Change == -2 ~ "- -",
      Change == -3 ~ "- - -",
      Change == -4 ~ "- - - -",
      Change == -5 ~ "- - - - -",
      Change == -6 ~ "- - - - -",
      Change == 1 ~ "+",
      Change == 2 ~ "+ +",
      Change == 3 ~ "+ + +",
      Change == 4 ~ "+ + + +",
      Change == 5 ~ "+ + + + +",
      Change == 6 ~ "+ + + + +"))
  
  gplot <- ggplot(df_merged, aes(x = LoD, y = Group_name, fill = Change)) +
    geom_tile(color = "white") +
    scale_fill_manual(values = c("#eec7fc","#3d405b" ,"#274c77", "#6096ba", "#a3cef1", "#e9c46a", "#f4a261", "#e76f51", "#660708", "#ffdce0", "grey95"),
                      breaks = c("- - - - -", "- - - -","- - -", "- -", "-", "+", "+ +", "+ + +", "+ + + +", "+ + + + +", "0"),
                      labels = c("- - - - -", "- - - -","- - -", "- -", "-", "+", "+ +", "+ + +", "+ + + +", "+ + + + +", "no change"),
                      na.value = "white") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = NULL, y = NULL, fill = NULL, title = paste0("Difference between EwE ", period_1, " & EwE ", period_2)) +
    coord_fixed() +
    scale_x_discrete(position = "top") +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 9),
          strip.text = element_text(size = 14)) +
    facet_wrap(~ Species_focus)
  
  print(gplot)

}
