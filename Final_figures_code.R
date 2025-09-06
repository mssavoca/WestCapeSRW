##################
# First submission plots
##################

source("Util_SRW.R")



# Fig. 1; Map ----


# devtools::install_github("MikkoVihtakari/ggOceanMapsData") # required by ggOceanMaps
# devtools::install_github("MikkoVihtakari/ggOceanMaps")
# # 
# install.packages("ggOceanMapsData", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org"))
# 
# remotes::install_github("MikkoVihtakari/ggOceanMaps")
# library(raster)
# library(sf)
# library(ggOceanMapsData)
# library(ggOceanMaps)
# library(ggspatial)


country_borders <- st_read("DoS_LSIB_v11_3_19Dec2023.shp")

dt <- data.frame(lon = c(9, 9, 27, 27), lat = c(-20, -20, -37, -37))

BoxFieldwork <- data.frame(
  lon = c(17.5, 17.5, 18.6, 18.6),
  lat = c(-33.8,-32.3, -32.3,  -33.8)
)

SRW_regional_map <- basemap(data = dt 
                            #bathymetry = TRUE, 
                            #bathy.style = "rcb"
) + 
  geom_sf(data = country_borders, color = "black") +
  xlim(14, 25) +
  ylim(-37,-25) +
  coord_sf(expand = F) +
  ggspatial::geom_spatial_polygon(
    data = BoxFieldwork,
    aes(x = lon, y = lat),
    fill = NA, color = "black", size = 1.25) +
  # ggspatial::geom_spatial_polygon(
  #   data = BoxWalvisBG, 
  #   aes(x = lon, y = lat), 
  #   fill = NA, color = "darkviolet", size = 1.25) +
  # ggspatial::geom_spatial_polygon(
  #   data = BoxSABG, 
  #   aes(x = lon, y = lat), 
  #   fill = NA, color = "darkviolet", size = 1.25) +
  # annotate("text", x = c(15.85,21.25), y = c(-22.8,-33.8), 
  #          label = c("Walvis Bay,\nNamibia","Cape Town, South Africa"), 
  #          size = 4) +
  annotation_scale(location = "br", line_width = 3, text_cex = 1.5) + 
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1.75, "cm"),
                         width = unit(1.75, "cm")) +
  theme(text = element_text(size = 28))
SRW_regional_map

dt_fieldwork =  data.frame(lon = c(17.3, 17.3, 18.35, 18.35), lat = c(-33.7,-32.4, -32.4,  -33.7))

whale_encouters = read.csv("encounters.csv")
prey_stations = read.csv("stations.csv")
prey_target = read.csv("target.csv")
tagging_loc = read_xlsx("Tag_Guide_SRW.xlsx")

# Standardize column names
whale_encouters <- whale_encouters %>% rename(lon = long)
prey_stations <- prey_stations %>% rename(lon = long)
tagging_loc <- tagging_loc %>% rename(lon = Long_On, lat = Lat_On)

# Plot with consistent 'lon' and 'lat'
Fieldwork_map <- basemap(data = dt_fieldwork) +
  # geom_point(data = whale_encouters, 
  #            aes(x = lon, y = lat, color = period), size = 3.5, alpha = 0.7) +
  geom_point(data = prey_target, 
             aes(x = lon, y = lat, color = period), shape = 2, size = 3.5) +
  geom_point(data = prey_stations, 
             aes(x = lon, y = lat), shape = 3, color = "darkred", size = 3.5) +
  geom_point(data = tagging_loc, 
             aes(x = lon, y = lat), shape = 23, color = "blue", size = 3.5) +
  scale_color_manual(values = c("hist" = "orange2", "new" = "darkgreen", "old" = "orange2")) +
  labs(color = "Period") +
  annotation_scale(location = "br", line_width = 4, text_cex = 1.5) +
  scale_x_continuous(
    breaks = seq(floor(min(dt_fieldwork$lon, na.rm = TRUE)),
                 ceiling(max(dt_fieldwork$lon, na.rm = TRUE)), by = 0.5)
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(dt_fieldwork$lat, na.rm = TRUE)),
                 ceiling(max(dt_fieldwork$lat, na.rm = TRUE)), by = 0.5)
  ) +
  theme_bw(base_size = 22) +
  theme(legend.position = "none")

Fieldwork_map



#Fig. 2; Zooplankton sampling, stacked bar plots----

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




# Fig. 3; prey hauls plot ----

Zoop_data <- read.csv("Table S2_Zooplankton sampling data.csv")


# Reshape to long format and rename Family levels
Zoop_long <- Zoop_data %>%
  pivot_longer(
    cols = c(cal_dens, small_dens, euc_dens, met_dens, 
             cen_dens, can_dens, oth_dens, eup_dens, biv_dens),
    names_to = "Family",
    values_to = "density"
  ) %>%
  mutate(
    Family = recode(Family,
                    cal_dens   = "Calanidae",
                    small_dens = "Small copepods",
                    euc_dens   = "Eucalanidae",
                    met_dens   = "Metridinidae",
                    cen_dens   = "Centropagidae",
                    can_dens   = "Candaciidae",
                    oth_dens   = "Other",
                    eup_dens   = "Euphausiidae",
                    biv_dens   = "Bivalvia"
    ),
    Family = factor(Family, levels = c("Calanidae", "Small copepods", "Metridinidae",
                                       "Bivalvia", "Centropagidae", "Candaciidae",
                                       "Eucalanidae", "Euphausiidae", "Other")),
  )

# Calculate medians for each Family × Station_or_target
median_lines <- Zoop_long %>%
  group_by(Family, Station_or_target) %>%
  summarise(med_density = median(density, na.rm = TRUE), .groups = "drop")

Prey_hauls_plot_R1 <- ggplot(Zoop_long, aes(x = Station_or_target, y = density, color = Station_or_target)) +
  geom_violin(trim = FALSE, fill = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  # dashed median lines
  geom_hline(data = median_lines,
             aes(yintercept = med_density, color = Station_or_target),
             linetype = "dashed", size = 0.75) +
  scale_y_log10(
    limits = c(1, 100000),
    labels = label_comma(),
    breaks = trans_breaks("log10", function(x) 10^x)  # every 10^n
  ) +
  scale_fill_manual(values = c("target" = "blue", "station" = "red")) +
  scale_color_manual(values = c("target" = "blue", "station" = "red")) +
  facet_grid(. ~ Family, scales = "free_y") +
  labs(
    x = "Sample type",
    y = expression("Density (individuals per " * m^3 * ")")
  ) +
  theme_bw(base_size = 15) +
  theme(
    strip.text = element_text(size = 11),
    legend.position = "none"
  )
Prey_hauls_plot_R1

# Save or display the figure
ggsave("Prey hauls plot_R1.pdf", Prey_hauls_plot_R1, width = 12.25, height = 5, units = "in")


# Code for Fig S1
median_lines_forRev3 <- Zoop_long %>%
  filter(year > 2021) %>% 
  group_by(Family, Station_or_target) %>%
  summarise(med_density = median(density, na.rm = TRUE), .groups = "drop")


Prey_hauls_plot_R1_forRev3 <- ggplot(
  filter(Zoop_long, year > 2021),
         aes(x = Station_or_target, y = density, color = Station_or_target)) +
  geom_violin(trim = FALSE, fill = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
  # dashed median lines
  geom_hline(data = median_lines_forRev3,
             aes(yintercept = med_density, color = Station_or_target),
             linetype = "dashed", size = 0.75) +
  scale_y_log10(
    limits = c(1, 100000),
    labels = label_comma(),
    breaks = trans_breaks("log10", function(x) 10^x)  # every 10^n
  ) +
  scale_fill_manual(values = c("target" = "blue", "station" = "red")) +
  scale_color_manual(values = c("target" = "blue", "station" = "red")) +
  facet_grid(. ~ Family, scales = "free_y") +
  labs(
    x = "Sample type",
    y = expression("Density (individuals per " * m^3 * ")")
  ) +
  theme_bw(base_size = 15) +
  theme(
    strip.text = element_text(size = 11),
    legend.position = "none"
  )
Prey_hauls_plot_R1_forRev3

ggsave("Prey_hauls_plot_R1_forRev3.pdf", Prey_hauls_plot_R1_forRev3, width = 12.25, height = 5, units = "in")



# # Convert "month" column to factor with desired order
# All_prey_density <- All_prey_density %>%
#   mutate(month = factor(month, levels = c("October", "November", "December", "January")))
# 
# # Filter out rows with "September" in the "month" column
# All_prey_density_filtered <- All_prey_density %>%
#   filter(month != "September")
# 
# All_prey_dens_bytaxa <- All_prey_density_filtered %>%
#   mutate(
#     dens_by_m3 = dens_by_m3 + 1,
#     taxa = recode(taxa, "other" = "Other"),
#     # Custom factor levels with Calanidae first and Other last
#     taxa = factor(taxa, levels = c("Calanidae", "Small copepods", "Metridinidae",
#                                    "Bivalvia", "Centropagidae", "Candaciidae",
#                                    "Eucalanidae", "Euphausiidae", "Other")),  # REPLACE WITH YOUR ACTUAL TAXA NAMES
#     # Order station_type with target on top
#     station_type = factor(station_type) %>% fct_relevel("target", "station")
#   ) %>%
#   ggplot(aes(x = dens_by_m3, y = station_type, fill = station_type)) +
#   geom_density_ridges(
#     aes(point_color = station_type),
#     point_alpha = 0.2,
#     jittered_points = TRUE, 
#     alpha = 0.5, 
#     scale = 2.5,
#     position = position_raincloud(height = 0.3, ygap = 0.05)
#   ) +
#   facet_wrap(~ taxa, scales = "free_y", ncol = 3, strip.position = "top") +  # Use factor ordering
#   theme_bw() +
#   labs(x = expression("Density (individuals per " * m^3 * ")"), y = NULL) +
#   scale_x_continuous(
#     trans = "log10",
#     breaks = 10^(seq(0, 6, 2)),
#     labels = scales::trans_format("log10", math_format(10^.x))
#   ) + 
#   theme(
#     strip.text = element_text(size = 10, face = "bold"),
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     legend.position = "right"
#   ) +
#   scale_y_discrete(expand = expansion(mult = c(0.25, 2.75))) +
#   scale_discrete_manual(
#     aesthetics = "point_color", 
#     values = c("target" = "blue", "station" = "red"),
#     name = "Sample Type"  # Unified legend title
#   ) +
#   scale_fill_manual(
#     values = c("target" = "blue", "station" = "red"),
#     name = "Sample Type",  # Unified legend title
#     labels = c("Target", "Station")
#   )
# 
# All_prey_dens_bytaxa


# ggsave("All_prey_dens_by_taxa_wdots.pdf", 
#        width = 6, height = 5, units = "in")   



#Fig. 6; Dive details plot----

library(ggpubr)

# 1. Show the fill legend in panel A (depth_plot)
depth_plot <- ggplot(SRW_dive_data, aes(x = avg_feeding_depth, fill = deployment)) +
  geom_density_ridges(aes(x = avg_feeding_depth, y = deployment), scale = 3.2, alpha = 0.5, color = "black") +
  scale_x_reverse(limits = c(70, -2)) +
  scale_y_discrete(expand = expansion(mult = c(0, 0.3))) +
  coord_flip() +
  labs(x = "Avg. Feeding Depth (m)", y = "Feeding Likelihood", fill = "Deployment") +
  theme_classic(base_size = 12) +
  scale_fill_manual(values = c(
    "ea230116-P47" = "#332288", "ea230119-P49" = "#88CCEE", "ea230120-P46" = "#44AA99",
    "ea230120-P48a"= "#117733", "ea230120-P48b"= "#999933", "ea250124-40" = "#DDCC77",
    "ea250124-82" = "#CC6677", "ea250124-P46"= "#882255", "ea250124-P48" = "#AA4499",
    "ea250131-40"= "#CC3311")) +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.text = element_text(size = 9),
        legend.key = element_blank(),
        legend.background = element_blank())
# (No guides(fill = "none") here!)

# 2. Hide the deployment legend in panels B and C
timeofday_plot <- timeofday_plot + guides(color = "none")
feedingdepth_plot <- feedingdepth_plot + guides(color = "none")



# Make sure all plots use the same fill/color mapping and have their legends enabled.

# Combine plots with a common legend at the top and panel labels
fig6 <- ggarrange(
  depth_plot, timeofday_plot, feedingdepth_plot,
  nrow = 1,
  labels = c("A", "B", "C"),
  font.label = list(size = 14, face = "bold"),
  align = "hv",
  common.legend = TRUE,
  legend = "top"
)

# Save or display the figure
ggsave("Feeding_plots_labeled.pdf_R1.pdf", fig6, width = 12, height = 4, units = "in")
print(fig6)






# Fig. 7; Dive kinematics plot----

tort_plot_ridge <- SRW_dive_data %>%
  #filter(!is.nan(tortuosity_feeding)) %>% # Drop out dives where we don't have this
  mutate(position = as.numeric(factor(deployment, levels = sort(unique(deployment))))) %>%
  mutate(group = "All") %>% # Create a dummy variable for plotting all
  mutate(deployment = factor(deployment, levels = rev(sort(unique(deployment))))) %>%
  ggplot(aes(x = tortuosity_feeding, y = position, fill = deployment)) +
  geom_density_ridges(scale = 2, alpha = 0.6, color = "black", rel_min_height = 0.01) +
  geom_rug(aes(x = tortuosity_feeding), sides = "b", alpha = 0.3) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.1)))+  # increase top space
  labs(x = "Avg. Feeding Tortuosity", y = NULL, fill = "Deployment") +
  geom_density_ridges(aes(x = tortuosity_feeding, y = 12.25), 
                      fill = "gray80", alpha = 0.5, scale = 1.2) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),  # Remove major vertical gridlines
    panel.grid.minor.x = element_blank()
  ) + 
  geom_hline(yintercept = c(1:10, 12.25), color = "black", linetype = "solid", alpha =0.25) +  # Add horizontal lines from y = 1 to y = 10
  scale_fill_manual(values = c("ea230116-P47" = "#332288",  # Assign custom colors to each group
                               "ea230119-P49" = "#88CCEE", 
                               "ea230120-P46" = "#44AA99",
                               "ea230120-P48a"= "#117733",
                               "ea230120-P48b"= "#999933",
                               "ea250124-40" = "#DDCC77" ,
                               "ea250124-82" = "#CC6677",
                               "ea250124-P46"= "#882255",
                               "ea250124-P48" = "#AA4499",
                               "ea250131-40"= "#CC3311")) 

tort_plot_ridge



speed_plot_ridge <- SRW_dive_data %>%
  #filter(!is.nan(avg_feeding_speed)) %>% # Drop out dives where we don't have this
  mutate(position = as.numeric(factor(deployment, levels = sort(unique(deployment))))) %>%
  mutate(group = "All") %>% # Create a dummy variable for plotting all
  mutate(deployment = factor(deployment, levels = rev(sort(unique(deployment))))) %>%
  ggplot(aes(x = avg_feeding_speed, y= position, fill = deployment)) +
  geom_density_ridges(scale = 2, alpha = 0.6, color = "black", rel_min_height = 0.001) +
  geom_rug(aes(x = avg_feeding_speed), sides = "b", alpha = 0.3) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.075)))+  # increase top space
  labs(x = "Avg. Feeding Speed (m/s)", y = NULL, fill = "Deployment") +
  geom_density_ridges(aes(x = avg_feeding_speed, y = 12.25), 
                      fill = "gray80", alpha = 0.5, scale = 1.2) +
  scale_x_continuous(limits = c(0.5, 2)) +  # Set the same x limits for both ridges
  theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),  # Remove major vertical gridlines
    panel.grid.minor.x = element_blank()
  ) + 
  geom_hline(yintercept = c(1:10, 12.25), color = "black", linetype = "solid", alpha =0.25) +  # Add horizontal lines from y = 1 to y = 10
  scale_fill_manual(values = c("ea230116-P47" = "#332288",  # Assign custom colors to each group
                               "ea230119-P49" = "#88CCEE", 
                               "ea230120-P46" = "#44AA99",
                               "ea230120-P48a"= "#117733",
                               "ea230120-P48b"= "#999933",
                               "ea250124-40" = "#DDCC77" ,
                               "ea250124-82" = "#CC6677",
                               "ea250124-P46"= "#882255",
                               "ea250124-P48" = "#AA4499",
                               "ea250131-40"= "#CC3311")) 



legend_right <- get_legend(
  speed_plot_ridge + theme(legend.position = "right")
)

# Remove legend from right plot before combining
speed_plot_ridge_no_legend <- speed_plot_ridge + theme(legend.position = "none")

# Combine the four plots using ggarrange and add individual labels (A, B)
# combined_plot <- ggarrange(tort_plot_ridge, speed_plot_ridge,
#                            nrow = 1,
#                            #labels = c("A", "B"),  # Add labels A, B
#                            align = "h", 
#                            common.legend = TRUE,
#                            legend = "right",
#                            widths = c(1, 1))

# Combine the two ridge plots with labels A and B
combined_plot <- cowplot::plot_grid(
  cowplot::plot_grid(
    tort_plot_ridge, 
    speed_plot_ridge_no_legend, 
    ncol = 2, 
    align = "h", 
    labels = c("A", "B"),    # Add panel labels
    label_size = 14,         # Adjust label size as needed
    label_fontface = "bold"  # Bold labels
  ),
  legend_right,
  ncol = 2,
  rel_widths = c(1, 0.25)   # Adjust space for legend
)

combined_plot


# Save or display the figure
ggsave("Dive kinematics plot_R1.pdf", combined_plot, width = 11, height = 5, units = "in")

