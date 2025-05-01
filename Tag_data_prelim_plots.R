################################
# Prelim tagging analysis
################################

# Load some additional libraries
library(ggridges)
library(lubridate)

Tag_guide_SRW <- readxl::read_xlsx("Tag_Guide_SRW.xlsx") %>% 
  mutate(Total_tag_time = as.POSIXct(`Total Tag On Time HH:MM:SS _`, format = "%H:%M:%S", tz = "UTC", origin = "1970-01-01"))

  # mutate(Total_tag_time = as.POSIXct(`Total Tag On Time HH:MM:SS _`, format = "%H:%M:%S"))


#Dive depth by individual----
ggplot(SRW_dive_data, aes(x = avg_feeding_depth, color = deployment)) +
  geom_density(alpha = 0.8) + 
  geom_density(aes(x = avg_feeding_depth), color = "black", size = 1) +
  coord_flip() +
  scale_x_reverse() +
  geom_rug(aes(x = avg_feeding_depth, color = deployment), sides = "b", alpha = 0.7) +  # Adding rug plot at the bottom
  labs(x = "Feeding depth (m)", y = "Feeding likelihood") +
  theme_classic(base_size = 16)

ggsave("dive_depth_density_plot.pdf", 
       width = 6, height = 6, units = "in")


#Dive depth by individual w/ colored ridges----
depth_plot <- ggplot(SRW_dive_data, aes(x = avg_feeding_depth, fill = deployment)) +
    #geom_density(alpha = 0.8) + 
    geom_density_ridges(aes(x = avg_feeding_depth, y = deployment), scale = 3.2, alpha =0.5, color = "black") +
    scale_x_reverse(limits = c(70, -2)) + # Adds 5% space on the right) +
    scale_y_discrete(expand = expansion(mult = c(0, 0.3)))+
    coord_flip() +
    #geom_rug(aes(x = avg_feeding_depth, color = deployment), sides = "b", alpha = 0.7) +  # Adding rug plot at the bottom
    labs(x = "Avg. Feeding Depth (m)", y = "Feeding Likelihood", fill = "Deployment") +
    theme_classic(base_size = 12  )+
    scale_fill_manual(values = c("ea230116-P47" = "#332288",  # Assign custom colors to each group
                                "ea230119-P49" = "#88CCEE", 
                                "ea230120-P46" = "#44AA99",
                                "ea230120-P48a"= "#117733",
                                "ea230120-P48b"= "#999933",
                                "ea250124-40" = "#DDCC77" ,
                                "ea250124-82" = "#CC6677",
                                "ea250124-P46"= "#882255",
                                "ea250124-P48" = "#AA4499",
                                "ea250131-40"= "#CC3311")) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = c(0.8, 0.8), 
        legend.text=element_text(size=9), 
        legend.key = element_blank(),
        legend.background=element_blank()) + 
  guides(fill = "none") 

depth_plot

ggsave("dive_depth_density_plot.pdf", 
         width = 6, height = 6, units = "in")


#Dive depth by time of day----  
# Create the plot
ggplot(SRW_dive_data, aes(x = time, y = avg_feeding_depth, 
                          color = deployment, shape = dive_type)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(
    x = "Time of day",
    y = "Average Feeding Depth",
    shape = "dive type"
  ) +
  scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +  # Setting x-axis ticks every hour
  ylim(0,70) +
  scale_y_reverse() +
  scale_shape_manual(values = c("u" = 16, "s" = 15, "v" = 17),  # Assigning specific shapes
                     labels = c("Surface-feeding", "U-shaped dive", "V-shaped dive")) +
  
  theme_classic(base_size = 16)


ggsave("dive_depth_ToD_plot.pdf", 
       width = 8, height = 6, units = "in")  
  
  
#Dive depth by time of day w/ colored points----  
  # Create the plot
timeofday_plot <- ggplot(SRW_dive_data, aes(x = time, y = avg_feeding_depth, 
                            color = deployment, shape = dive_type)) +
    geom_point(alpha = 0.6, size = 2) +
    labs(
         x = "Time of Day",
         y = NULL,
         shape = "Dive Type"
         ) +
    scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +  # Setting x-axis ticks every hour
    ylim(-5,80) +
    scale_y_reverse() +
    scale_shape_manual(values = c("u" = 16, "s" = 15, "v" = 17),  # Assigning specific shapes
                       labels = c("Surface-feeding", "U-shaped dive", "V-shaped dive")) +
    scale_color_manual(values = c("ea230116-P47" = "#332288",  # Assign custom colors to each group
                                 "ea230119-P49" = "#88CCEE", 
                                 "ea230120-P46" = "#44AA99",
                                 "ea230120-P48a"= "#117733",
                                 "ea230120-P48b"= "#999933",
                                 "ea250124-40" = "#DDCC77" ,
                                 "ea250124-82" = "#CC6677",
                                 "ea250124-P46"= "#882255",
                                 "ea250124-P48" = "#AA4499",
                                 "ea250131-40"= "#CC3311")) +
    
    theme_classic(base_size = 12)+ 
    theme(legend.position = c(0.4, 0.6), legend.text=element_text(size=9))+
    guides(color="none")+ 
    ylim(70, -2)
 
  timeofday_plot  

  ggsave("dive_depth_ToD_plot.pdf", 
         width = 8, height = 6, units = "in")  


#Feeding time per dive by time of day----  
  
  # Create the plot
  ggplot(SRW_dive_data, aes(x = feeding_time, y = avg_feeding_depth, color = deployment)) +
    geom_point(alpha = 0.7) +
    geom_smooth(color = "black", method = "lm") +
    labs(
      x = "Feeding time (s)",
      y = "Average Feeding Depth") +
    #scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +  # Setting x-axis ticks every hour
    scale_y_reverse(limits = c(70,20)) +
    theme_classic(base_size = 16)
  
  
#Feeding time per dive by time of day w/ colored points ----  

  # Create the plot
  ggplot(SRW_dive_data, aes(x = feeding_time, y = avg_feeding_depth, color = deployment)) +
    geom_point(alpha = 0.7) +
    geom_smooth(color = "black", method = "lm") +
    labs(
         x = "Feeding time (s)",
         y = "Average Feeding Depth") +
    #scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +  # Setting x-axis ticks every hour
    scale_y_reverse(limits = c(70,20)) +
    theme_classic(base_size = 12) +
    scale_color_manual(values = c("ea230116-P47" = "#332288",  # Assign custom colors to each group
                                 "ea230119-P49" = "#88CCEE", 
                                 "ea230120-P46" = "#44AA99",
                                 "ea230120-P48a"= "#117733",
                                 "ea230120-P48b"= "#999933",
                                 "ea250124-40" = "#DDCC77" ,
                                 "ea250124-82" = "#CC6677",
                                 "ea250124-P46"= "#882255",
                                 "ea250124-P48" = "#AA4499",
                                 "ea250131-40"= "#CC3311"))
  
  
  

 # Feeding time (effort) by hour of the day----
  
  # Ensure the time column is in proper time format
  SRW_dive_data$time <- hms::as_hms(SRW_dive_data$time)
  
  # Extract the hour from the time column
  SRW_dive_data <- SRW_dive_data %>%
    mutate(hour = hour(time))
  
  # Summarize feeding_time by hour
  feeding_time_by_hour <- SRW_dive_data %>%
    group_by(hour) %>%
    summarise(total_feeding_time = sum(feeding_time, na.rm = TRUE))
  

# Plot number of dives per hour by average depth----
  # Summarize number of dives by hour for each deployment and add average feeding time
  dives_by_hour_and_deployment <- SRW_dive_data %>%
    mutate(hour = hour(time)) %>%  # Extract the hour from the "time" column
    group_by(deployment, hour) %>%  # Group by deployment and hour
    summarise(
      num_feeding_dives_per_h = n(),  # Count the number of dives (rows)
      mean_avg_feeding_depth = (mean = avg_feeding_depth),
      avg_feeding_time = mean(feeding_time, na.rm = TRUE),  # Calculate average feeding_time for each group
      sd_feeding_time = sd(feeding_time, na.rm = TRUE)
    ) %>% 
    mutate(se_feeding_time = sd_feeding_time/sqrt(num_feeding_dives_per_h)) %>% 
    arrange(deployment, hour)  # Arrange by deployment and hour
  
  # Plot number of dives by mean_avg_feeding_depth color by deployment
  ggplot(dives_by_hour_and_deployment, aes(x = num_feeding_dives_per_h, y = mean_avg_feeding_depth, 
                                           color = deployment, size = avg_feeding_time)) +
    geom_point(alpha = 0.3) +
    #geom_smooth(method = "lm", se = FALSE) +  # Optional: to add trend lines
    labs(
      #title = "Number of Dives by Mean Average Feeding Depth",
      y = "Mean Average Feeding Depth (m)",
      x = "Number of feeding dives per hour",
      size = "average feeding time\nper dive (s)"
    ) +
    ylim(0,70) +
    scale_y_reverse() +
    theme_classic(base_size = 16)
  
  ggsave("Feeding_rate_by_depth_plot.pdf", 
         width = 8, height = 6, units = "in") 
  
  # Plot number of dives by mean_avg_feeding_depth color by deployment w/ colored points
feedingdepth_plot <- ggplot(dives_by_hour_and_deployment, aes(x = num_feeding_dives_per_h, y = mean_avg_feeding_depth, 
                                           color = deployment, size = avg_feeding_time)) +
    geom_point(alpha = 0.3) +
    #geom_smooth(method = "lm", se = FALSE) +  # Optional: to add trend lines
    labs(
      #title = "Number of Dives by Mean Average Feeding Depth",
      y = NULL,
      x = "# of feeding dives/hour",
      size = "Avg. Feeding \nTime per Dive (s)"
    ) +
    ylim(-2,70) +
    scale_y_reverse() +
    theme_classic(base_size = 12) +
  guides(color = "none")+
  scale_color_manual(values = c("ea230116-P47" = "#332288",  # Assign custom colors to each group
                                "ea230119-P49" = "#88CCEE", 
                                "ea230120-P46" = "#44AA99",
                                "ea230120-P48a"= "#117733",
                                "ea230120-P48b"= "#999933",
                                "ea250124-40" = "#DDCC77" ,
                                "ea250124-82" = "#CC6677",
                                "ea250124-P46"= "#882255",
                                "ea250124-P48" = "#AA4499",
                                "ea250131-40"= "#CC3311"))+
  theme(legend.position = c(0.7, 0.3), legend.text=element_text(size=9))

feedingdepth_plot

ggsave("Feeding_rate_by_depth_plot.pdf", 
       width = 8, height = 6, units = "in")    

# Plot all together
cowplot::plot_grid(depth_plot, timeofday_plot, feedingdepth_plot,  nrow = 1, align = 'hv')
  
ggsave("Feeding_plots.pdf", 
       width = 9, height = 3.5, units = "in")    

  


#Movement behaviors while feeding plot----

# Load necessary packages
library(ggplot2)
library(ggpubr)
library(ggridges)
library(cowplot)

# Define all deployments you want in the legend
all_deployments <- c("ea230116-P47", "ea230119-P49", "ea230120-P46", "ea230120-P48a", 
                     "ea230120-P48b", "ea250124-40", "ea250124-82", "ea250124-P46", 
                     "ea250124-P48", "ea250131-40")

SRW_dive_data <- SRW_dive_data %>%
  mutate(deployment = factor(deployment, levels = all_deployments))

# Plot A: Tortuosity
tort_plot <- ggplot(SRW_dive_data, aes(x = tortuosity_feeding, color = deployment)) +
  geom_density(alpha = 0.6) +
  geom_density(color = "black", size = 1) +
  geom_rug(aes(x = tortuosity_feeding), sides = "b", alpha = 0.3) +
  labs(x = "Tortuosity while feeding", y = "Density") +
  #scale_fill_brewer(palette = "Set2") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

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

# Plot B: Avg Feeding Speed
speed_plot <- ggplot(SRW_dive_data, aes(x = avg_feeding_speed, color = deployment)) +
  geom_density(alpha = 0.6) +
  geom_density(color = "black", size = 1) +
  geom_rug(aes(x = avg_feeding_speed), sides = "b", alpha = 0.3) +
  labs(x = "Avg speed while feeding (m/s)", y = "Density") +
  #scale_fill_brewer(palette = "Set2") +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom")

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

combined_plot <- cowplot::plot_grid(
  plot_grid(tort_plot_ridge, speed_plot_ridge_no_legend, ncol = 2, align = "h"),
  legend_right,
  ncol = 2,
  rel_widths = c(1, 0.25)  # Adjust space for legend
)

# Display the combined plot
print(combined_plot)


ggsave("Feeding_tort and speed_plot_ridges.pdf", 
       width = 8, height = 5, units = "in")   










  
# Scatter plot of time_between_dives and feeding_time
  ggplot(SRW_dive_data, aes(x = feeding_time, y = time_between_dives)) +
    geom_point(aes(color = deployment), alpha = 0.7) +
    geom_smooth(color = "black") +
    labs(title = "Scatter Plot of Time Between Dives and Feeding Time",
         x = "Feeding Time (seconds)",
         y = "Time Between Dives (seconds)") +
    ylim(0,500) +
    theme_classic(base_size = 16)

  
  
  
  




# Numbers of feeding metrics for paper ----

dives_by_hour_and_deployment %>%
  filter(mean_avg_feeding_depth >30) %>%
  ungroup() %>%  # Ensure there's no grouping
  summarise(
    median_dives_per_h = median(num_feeding_dives_per_h, na.rm = TRUE),
    sd_dives_per_h = sd(num_feeding_dives_per_h, na.rm = TRUE)
  )

median(SRW_dive_data$avg_feeding_speed)
sd(SRW_dive_data$avg_feeding_speed)

median(SRW_dive_data$stroke_frequency_feeding)
sd(SRW_dive_data$stroke_frequency_feeding)

  
SRW_dive_data %>%
  filter(avg_feeding_depth < 10) %>%
  ungroup() %>%  # Ensure there's no grouping
  summarise(median_feeding_time = median(feeding_time, na.rm = TRUE),
            sd_feeding_time = sd(feeding_time, na.rm = TRUE),
            median_feeding_speed = median(avg_feeding_speed, na.rm = TRUE),
            sd_feeding_speed = sd(avg_feeding_speed, na.rm = TRUE),
  )




# EXPERIMENTAL below here ----




#Kinematics of feeding plot----

# Load necessary packages
library(ggplot2)
library(ggpubr)

# Plot A: Pitch
pitch_plot <- ggplot(SRW_dive_data, aes(x = avg_pitch_feeding, color = deployment)) +
  geom_density(alpha = 0.6) +
  geom_density(color = "black", size = 1) +
  geom_rug(aes(x = avg_pitch_feeding), sides = "b", alpha = 0.3) +
  labs(x = "Avg pitch", y = "Density") +
  #scale_fill_brewer(palette = "Set2") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

# Plot B: Roll
roll_plot <- ggplot(SRW_dive_data, aes(x = max_roll_feeding, color = deployment)) +
  geom_density(alpha = 0.6) +
  geom_density(color = "black", size = 1) +
  geom_rug(aes(x = max_roll_feeding), sides = "b", alpha = 0.3) +
  labs(x = "Max roll", y = "Density") +
  #scale_fill_brewer(palette = "Set2") +
  xlim(0,100) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

# Plot C: Heading
heading_plot <- ggplot(SRW_dive_data, aes(x = heading_excursion, color = deployment)) +
  geom_density(alpha = 0.6) +
  geom_density(color = "black", size = 1) +
  geom_rug(aes(x = heading_excursion), sides = "b", alpha = 0.3) +
  labs(x = "Heading excursion", y = "Density") +
  #scale_fill_brewer(palette = "Set2") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

# Plot D: Avg Feeding Speed
speed_plot <- ggplot(SRW_dive_data, aes(x = avg_feeding_speed, color = deployment)) +
  geom_density(alpha = 0.6) +
  geom_density(color = "black", size = 1) +
  geom_rug(aes(x = avg_feeding_speed), sides = "b", alpha = 0.3) +
  labs(x = "Avg speed (m/s)", y = "Density") +
  #scale_fill_brewer(palette = "Set2") +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom")

# Combine the four plots using ggarrange and add individual labels (A, B, C, D)
combined_plot <- ggarrange(pitch_plot, roll_plot, heading_plot, speed_plot,
                           ncol = 1, nrow = 4, 
                           labels = c("A", "B", "C", "D"),  # Add labels A, B, C, D
                           align = "v", 
                           common.legend = TRUE, legend = "bottom")

# Display the combined plot
print(combined_plot)


ggsave("Feeding_kinematics_plot.pdf", 
       width = 9, height = 9, units = "in")   

  
  # Ensure the time column is in proper time format
  SRW_dive_data$time <- hms::as_hms(SRW_dive_data$time)
  
# Combine the hour extraction, total feeding time calculation, and dive count in one pipeline
  feeding_time_and_dive_count_by_hour <- SRW_dive_data %>%
    mutate(hour_of_day = hour(time)) %>%  # Extract the hour from the "time" column
    group_by(hour_of_day) %>%  # Group by hour
    summarise(
      total_feeding_time = sum(feeding_time, na.rm = TRUE),  # Sum feeding time per hour
      number_of_dives = n()  # Count the number of rows (dives) per hour
    ) %>%
    arrange(hour_of_day)  # Arrange by hour
  
  
# Summarize number of dives by hour for each deployment
  dives_by_hour_and_deployment <- SRW_dive_data %>%
    mutate(hour = hour(time)) %>%  # Extract the hour from the "time" column
    group_by(deployment, hour) %>%  # Group by deployment and hour
    summarise(number_of_dives = n()) %>%  # Count the number of dives (rows) per deployment and hour
    arrange(deployment, hour)  # Arrange by deployment and hour
  

  
  
  
Feed_time_summ <-  SRW_dive_data %>% 
  group_by(dive_type) %>% 
  summarise(med_feed_dur = median(feeding_time),
            sd_feed_dur = sd(feeding_time))
Feed_time_summ
  
  








### AND do a three-panel pitch-roll-heading? density plot, as done for depth

  
  # Reshape the data for faceting and assign x-axis limits for each measurement
  pitch_roll_heading_data <- SRW_dive_data %>%
    select(deployment, avg_pitch_feeding, max_roll_feeding, heading_excursion) %>%
    pivot_longer(cols = c(avg_pitch_feeding, max_roll_feeding, heading_excursion),
                 names_to = "measurement", values_to = "value") %>%
    mutate(measurement = factor(measurement, levels = c("avg_pitch_feeding", "max_roll_feeding", "heading_excursion")),
           # Set custom limits for pitch, roll, and heading
           x_limits = case_when(
             measurement %in% c("avg_pitch_feeding", "max_roll_feeding") ~ list(c(-25, 160)),
             measurement == "heading_excursion" ~ list(c(-25, 450)),
             TRUE ~ list(c(min(value), max(value)))
           ))
  
  # Create density plots for pitch, roll, and heading with facet grid (3 rows)
  ggplot(pitch_roll_heading_data, aes(x = value, fill = deployment)) +
    geom_density(alpha = 0.6) +  # Density plot with transparency
    geom_rug(aes(x = value), sides = "b", alpha = 0.3) +  # Rug plot beneath all density plots
    facet_grid(rows = vars(measurement), scales = "free_y") +  # Facet by measurement, free y-axis scales
    labs(title = "Density Plots of Pitch, Roll, and Heading during Feeding",
         x = "Value", y = "Density") +
    scale_fill_brewer(palette = "Set2") +  # Set color palette
    theme_minimal(base_size = 14) + 
    theme(legend.position = "bottom") +
    # Apply x-axis limits using the x_limits column
    scale_x_continuous(limits = function(x) pitch_roll_heading_data$x_limits[[1]])
  
  
#Pitch, roll, heading while feeding----
  # Install and load the plotly package if not already installed
  install.packages("plotly")
  library(plotly)
  
  # Create a 3D scatter plot
  fig <- plot_ly(SRW_dive_data, 
                 x = ~avg_pitch_feeding, 
                 y = ~max_roll_feeding, 
                 z = ~heading_excursion, 
                 color = ~deployment,  # Coloring by 'deployment'
                 type = 'scatter3d', 
                 mode = 'markers', 
                 marker = list(size = 5, opacity = 0.7))
  
  # Add axis labels and a title
  fig <- fig %>% layout(title = "3D Scatter Plot: Avg Pitch, Max Roll, Heading Excursion",
                        scene = list(xaxis = list(title = 'Avg Pitch Feeding'),
                                     yaxis = list(title = 'Max Roll Feeding'),
                                     zaxis = list(title = 'Heading Excursion')))
  
  # Display the plot
  fig
  
  

