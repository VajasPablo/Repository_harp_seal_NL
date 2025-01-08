
plot_ecosystem_key_figure_function <- function(){

Ecosystem_scale_summary_table <- Ecosystem_scale_summary_table %>%
    mutate(EwE_Model = case_when(
      EwE_Model == "EwE 1985" ~ "1985-1987",
      EwE_Model == "EwE 2013" ~ "2013-2015",
      EwE_Model == "EwE 2020 2J3K" ~ "2018-2020 NL Shelf",
      EwE_Model == "EwE 2020 3LNO" ~ "2018-2020 Grand Banks",
      TRUE ~ EwE_Model  # keep other values unchanged
    ))

Ecosystem_scale_summary_table$order <- c(1, 2, 2.90, 3.1)

Ecosystem_scale_summary_table_1 <- Ecosystem_scale_summary_table %>% filter(order < 3)
Ecosystem_scale_summary_table_2 <- Ecosystem_scale_summary_table %>% filter(order %in% c(1, 2, 3.1))

# Ajoute une variable pour distinguer les deux ensembles de données
Ecosystem_scale_summary_table_1$dataset <- "Dataset 1"
Ecosystem_scale_summary_table_2$dataset <- "Dataset 2"

# Combine les deux tables en une seule
combined_table <- rbind(Ecosystem_scale_summary_table_1, Ecosystem_scale_summary_table_2)
combined_table <- combined_table %>% mutate(SOI = round(as.numeric(SOI), 4),
                                            Connectance = round(as.numeric(Connectance), 4),
                                            O_C = round(as.numeric(O_C), 4),
                                            TE = round(as.numeric(TE), 2),
                                            Total_biomass = round(as.numeric(Total_biomass), 2))

angle_x_axis = 35

# Trace le graphique en superposant les données
SOI_plot <- ggplot(combined_table, aes(x = order, y = SOI, group = dataset)) +
  geom_point(color = "black") +
  geom_line(color = "black") +
  scale_x_continuous(breaks = c(1, 2, 2.90, 3.1),
                     labels = unique(combined_table$EwE_Model)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(y = "System Omnivory Index") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = angle_x_axis, hjust = 1),
        legend.position = "none") +# ggtitle("SOI index") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12)) + ggtitle("a)")

Connectance_plot <- ggplot(combined_table, aes(x = order, y = Connectance, group = dataset)) +
  geom_point(color = "black") +
  geom_line(color = "black") +
  scale_x_continuous(breaks = c(1, 2, 2.90, 3.1),
                     labels = unique(combined_table$EwE_Model)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(y = "Connectance") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = angle_x_axis, hjust = 1),
        legend.position = "none") + #ggtitle("Connectance index") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12)) + ggtitle("b)")

O_C_plot <- ggplot(combined_table, aes(x = order, y = O_C, group = dataset)) +
  geom_point(color = "black") +
  geom_line(color = "black") +
  scale_x_continuous(breaks = c(1, 2, 2.90, 3.1),
                     labels = unique(combined_table$EwE_Model)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(y = "Overhead/Capacity") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = angle_x_axis, hjust = 1),
        legend.position = "none") + #ggtitle("O/C index") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12)) + ggtitle("c)")

TE_plot <- ggplot(combined_table, aes(x = order, y = TE, group = dataset)) +
  geom_point(color = "black") +
  geom_line(color = "black") +
  scale_x_continuous(breaks = c(1, 2, 2.90, 3.1),
                     labels = unique(combined_table$EwE_Model)) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  labs(y = "Ecosystem Transfer Efficiency") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = angle_x_axis, hjust = 1),
        legend.position = "none") + #ggtitle("O/C index") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12)) + ggtitle("d)")


TB_plot <- ggplot(combined_table, aes(x = order, y = Total_biomass, group = dataset)) +
  geom_point(color = "black") +
  geom_line(color = "black") +
  scale_x_continuous(breaks = c(1, 2, 2.90, 3.1),
                     labels = unique(combined_table$EwE_Model)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6),
                     limits = c(0, 300)) +
  labs(y = "Total Ecosystem Biomass (t/km2)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = angle_x_axis, hjust = 1),
        legend.position = "none") + #ggtitle("O/C index") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12)) + ggtitle("e)")

grid.arrange(SOI_plot, Connectance_plot, 
             O_C_plot, TE_plot, TB_plot, ncol = 3)

}
