##################
# Prey data plots
##################

source("Util_SRW.R")



#Historic prey data plot----

# Calculate median by taxa for station type
median_by_taxa <- Historic_prey_dens %>%
  filter(station_type == "station") %>%
  group_by(taxa) %>%
  summarise(median_dens_by_m3 = median(dens_by_m3, na.rm = TRUE)) %>%
  arrange(desc(median_dens_by_m3))

# Reorder taxa based on median dens_by_m3 for station type
Hist_prey_dens <- ggplot(Historic_prey_dens, aes(x = reorder(taxa, -dens_by_m3, FUN = median), y = dens_by_m3, fill = station_type)) +
  geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.7, outlier.shape = NA) +
  geom_point(data = Historic_prey_dens[order(Historic_prey_dens$dens_by_m3),], 
             aes(color = station_type),
             position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.3), alpha = 0.2) +
  annotation_logticks(sides = "l") +
  scale_y_log10(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_manual(values = c("target" = "blue", "station" = "red")) +
  scale_color_manual(values = c("target" = "blue", "station" = "red")) +  # Match point color to fill color
  labs(x = "Taxa", y = expression("Density (individuals per " * m^3 * ")"), fill = "Station Type", color = "Station Type") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(1, 50000))

Hist_prey_dens



# Reorder taxa based on median dens_by_m3 for station type
Hist_prey_dens_month <- ggplot(Historic_prey_dens, aes(x = reorder(taxa, -dens_by_m3, FUN = median), y = dens_by_m3, fill = station_type)) +
  geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.7, outlier.shape = NA) +
  geom_point(data = Historic_prey_dens[order(Historic_prey_dens$dens_by_m3),], 
             aes(color = station_type),
             position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.3), alpha = 0.2) +
  scale_y_log10(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_manual(values = c("target" = "blue", "station" = "red")) +
  scale_color_manual(values = c("target" = "blue", "station" = "red")) +  # Match point color to fill color
  labs(x = "Taxa", y = expression("Density (individuals per " * m^3 * ")"), fill = "Station Type", color = "Station Type") +
  theme_minimal(base_size = 18) +
  coord_cartesian(ylim = c(1, 50000)) +
  facet_wrap(.~month)

Hist_prey_dens_month


#Modern prey data plot----

# Calculate median by taxa for station type
median_by_taxa_mod <- Modern_prey_dens %>%
  filter(station_type == "station") %>%
  group_by(taxa) %>%
  summarise(median_dens_by_m3 = median(dens_by_m3, na.rm = TRUE)) %>%
  arrange(desc(median_dens_by_m3))

# Reorder taxa based on median dens_by_m3 for station type
Mod_prey_dens <- ggplot(Modern_prey_dens, aes(x = reorder(taxa, -dens_by_m3, FUN = median), y = dens_by_m3, fill = station_type)) +
  geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.7, outlier.shape = NA) +
  geom_point(data = Modern_prey_dens[order(Modern_prey_dens$dens_by_m3),], 
             aes(color = station_type),
             position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.3), alpha = 0.2) +
  annotation_logticks(sides = "l") +
  scale_y_log10(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_manual(values = c("target" = "blue", "station" = "red")) +
  scale_color_manual(values = c("target" = "blue", "station" = "red")) +  # Match point color to fill color
  labs(x = "Taxa", y = expression("Density (individuals per " * m^3 * ")"), fill = "Station Type", color = "Station Type") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(1, 50000))

Mod_prey_dens



#All prey data plot----
All_prey_dens <- ggplot(All_prey_density, aes(x = reorder(taxa, -dens_by_m3, FUN = median), y = dens_by_m3, fill = station_type)) +
  geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.7, outlier.shape = NA) +
  geom_point(data = All_prey_density[order(All_prey_density$dens_by_m3),], 
             aes(color = station_type),
             position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.3), alpha = 0.2) +
  annotation_logticks(sides = "l") +
  scale_y_log10(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_manual(values = c("target" = "blue", "station" = "red")) +
  scale_color_manual(values = c("target" = "blue", "station" = "red")) +  # Match point color to fill color
  labs(x = "Taxa", y = expression("Density (individuals per " * m^3 * ")"), fill = "Station Type", color = "Station Type") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(1, 50000))
All_prey_dens




# Convert "month" column to factor with desired order
All_prey_density <- All_prey_density %>%
  mutate(month = factor(month, levels = c("October", "November", "December", "January")))

# Filter out rows with "September" in the "month" column
All_prey_density_filtered <- All_prey_density %>%
  filter(month != "September")

# Create the plot with facet_wrap
All_prey_dens_month <- ggplot(All_prey_density_filtered, aes(x = reorder(taxa, -dens_by_m3, FUN = median), y = dens_by_m3, fill = station_type)) +
  geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.7, outlier.shape = NA) +
  geom_point(data = All_prey_density_filtered[order(All_prey_density_filtered$dens_by_m3),], 
             aes(color = station_type),
             position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.3), alpha = 0.2) +
  annotation_logticks(sides = "l") +
  scale_y_log10(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_fill_manual(values = c("target" = "blue", "station" = "red")) +
  scale_color_manual(values = c("target" = "blue", "station" = "red")) +  # Match point color to fill color
  facet_wrap(~month, nrow = 2) +  # Facet by month with 1 column
  labs(x = "Taxa", y = expression("Density (individuals per " * m^3 * ")"), fill = "Station Type", color = "Station Type") +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(1, 50000))

All_prey_dens_month

ggsave("All_prey_dens_by_month_plot.pdf", 
       width = 9, height = 6, units = "in")    


# All prey dens by taxa by month plot
All_prey_dens_month_bytaxa <- All_prey_density_filtered %>%
  mutate(dens_by_m3 = dens_by_m3 +1) %>% # To deal with lots of zeros!
  mutate(taxa = recode(taxa, "other" = "Other"))%>%
  #mutate(month = factor(month, levels = c("October", "November", "December", "January"))) %>%
  ggplot(aes(x = dens_by_m3, y = station_type, fill = station_type)) +
  geom_density_ridges(aes(point_color = station_type, point_fill = station_type),
                      point_alpha = 0.75, jittered_points = TRUE, alpha = 0.4, scale = 2.5, rel_min_height = 0.01) +
  #geom_boxplot(position = position_dodge(width = 0.75), alpha = 0.7, outlier.shape = NA) +
  #geom_point(aes(color = station_type) ,
             #position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.3), alpha = 0.2) +
  facet_wrap(~ reorder(taxa, -dens_by_m3, FUN = median), scales = "free_y", ncol = 3, strip.position = "top") +  # Vertical stacking
  theme_bw() +
  labs(x = expression("Density (individuals per " * m^3 * ")"), y = NULL, fill = "Station Type") +
  #scale_x_log10() +
  scale_x_continuous(
    trans = "log10",
    breaks = 10^(seq(0, 6, 2)),
    labels = scales::trans_format("log10", math_format(10^.x))) + 
  theme(
    # Remove y-axis labels inside facets
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_blank(),  # Remove y-axis text inside the plot
    axis.ticks.y = element_blank(), # Remove y-axis ticks
  )+
  scale_y_discrete(expand = expansion(mult = c(0.25, 2.75)), labels = c("station" = "Station", "target" = "Target"))+  # increase top space
  scale_discrete_manual(aesthetics = "point_color", values = c("red", "blue"), name= "Prey Sample Type") +
  scale_fill_manual(values = c("target" = "blue", "station" = "red"), labels = c("Station", "Target"), name= "Prey Sample Type") +
  guides(fill = guide_legend("Prey Sample Type"), color = guide_legend("Prey Sample Type"))


All_prey_dens_month_bytaxa 


ggsave("All_prey_dens_by_taxa_plot_wdots.pdf", 
       width = 6, height = 5, units = "in")    








# Prey by taxa, raincloud plot ----

All_prey_dens_bytaxa <- All_prey_density_filtered %>%
  mutate(
    dens_by_m3 = dens_by_m3 + 1,
    taxa = recode(taxa, "other" = "Other"),
    # Custom factor levels with Calanidae first and Other last
    taxa = factor(taxa, levels = c("Calanidae", "Small copepods", "Metridinidae",
                                   "Bivalvia", "Centropagidae", "Candaciidae",
                                   "Eucalanidae", "Euphausiidae", "Other")),  # REPLACE WITH YOUR ACTUAL TAXA NAMES
    # Order station_type with target on top
    station_type = factor(station_type) %>% fct_relevel("target", "station")
  ) %>%
  ggplot(aes(x = dens_by_m3, y = station_type, fill = station_type)) +
  geom_density_ridges(
    aes(point_color = station_type),
    point_alpha = 0.2,
    jittered_points = TRUE, 
    alpha = 0.5, 
    scale = 2.5,
    position = position_raincloud(height = 0.3, ygap = 0.05)
  ) +
  facet_wrap(~ taxa, scales = "free_y", ncol = 3, strip.position = "top") +  # Use factor ordering
  theme_bw() +
  labs(x = expression("Density (individuals per " * m^3 * ")"), y = NULL) +
  scale_x_continuous(
    trans = "log10",
    breaks = 10^(seq(0, 6, 2)),
    labels = scales::trans_format("log10", math_format(10^.x))
  ) + 
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "right"
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.25, 2.75))) +
  scale_discrete_manual(
    aesthetics = "point_color", 
    values = c("target" = "blue", "station" = "red"),
    name = "Sample Type"  # Unified legend title
  ) +
  scale_fill_manual(
    values = c("target" = "blue", "station" = "red"),
    name = "Sample Type",  # Unified legend title
    labels = c("Target", "Station")
  )

All_prey_dens_bytaxa


ggsave("All_prey_dens_by_taxa_wdots.pdf", 
       width = 6, height = 5, units = "in")    


# Prey data summary table----
prey_summary_table <- All_prey_density %>%
  dplyr::group_by(taxa, station_type, Calanoida) %>%
  summarize(median_dens_by_m3 = median(dens_by_m3),
            sd_dens_by_m3 = sd(dens_by_m3)) %>% 
  arrange(-median_dens_by_m3)

prey_summary_table

#write_csv(prey_summary_table, "prey_summary_table.csv")


# Calculate total median density by m^3 for each station_type
total_median_density <- prey_summary_table %>%
  group_by(station_type) %>%
  summarize(total_median_dens_by_m3 = sum(median_dens_by_m3, na.rm = TRUE))

# Merge total median density back to the original table
prey_summary_table <- prey_summary_table %>%
  left_join(total_median_density, by = "station_type") %>%
  mutate(proportion = median_dens_by_m3 / total_median_dens_by_m3)

prey_summary_table_calanoida <- prey_summary_table %>% 
  group_by(Calanoida, station_type) %>% 
  summarise(prop_calanoid = sum(proportion))





#Proportional prey stacked bar plot----


# Calanoid v. non-calanoid 

PropCalanoid_plot <- ggplot(prey_summary_table_calanoida, aes(x = station_type, y = prop_calanoid, fill = Calanoida)) +
  geom_bar(stat = "identity", position = "stack") +  # Use stacked bars
  scale_fill_manual(
    values = c("Y" = "#56B4E9", "N" = "darkred"),  # Custom colors for "Y" and "N"
    labels = c("Y" = "Calanoid copepods", "N" = "Other taxa")  # Updated labels
  ) +
  labs(x = "Station Type", y = "Proportion of Calanoid copepods", fill = "Taxa") +  # Change the fill legend title
  theme_classic(base_size = 16) +
  theme(legend.position = "right")

# Display the plot
PropCalanoid_plot

ggsave("PropCalanoid.pdf", 
       width = 4.5, height = 9, units = "in")   


# Reorder the levels of the "taxa" variable based on the sorted order
prey_summary_table$taxa <- factor(prey_summary_table$taxa, 
                                   levels = rev(unique(prey_summary_table$taxa)))


# Set color palette to "glasbey"
glasbey_colors <- pals::glasbey(12)

# Plot the data with glasbey color palette
prey_overall_comp <- ggplot(prey_summary_table, 
                            aes(x = station_type, y = proportion, fill = taxa)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Station Type", y = "Proportional representation in prey hauls", fill = "Taxa") +
  theme_classic(base_size = 16) +
  scale_fill_manual(values = glasbey_colors)

prey_overall_comp


ggsave("prey_overall_comp.pdf", 
       width = 4.5, height = 9, units = "in") 





# Combine the four plots using ggarrange and add individual labels (A, B)
combined_prey_prop_plot <- ggarrange(PropCalanoid_plot, prey_overall_comp,
                           ncol = 2, nrow = 1, 
                           labels = c("A", "B"),  # Add labels A, B
                           align = "h")
combined_prey_prop_plot

ggsave("combined_prey_prop_plot.pdf", 
       width = 11, height = 6.5, units = "in") 
