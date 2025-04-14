################################
# Prelim tagging analysis
################################

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
  
  


#Movement behaviors while feeding plot----

# Load necessary packages
library(ggplot2)
library(ggpubr)

# Plot A: Tortuosity
tort_plot <- ggplot(SRW_dive_data, aes(x = tortuosity_feeding, color = deployment)) +
  geom_density(alpha = 0.6) +
  geom_density(color = "black", size = 1) +
  geom_rug(aes(x = tortuosity_feeding), sides = "b", alpha = 0.3) +
  labs(x = "Tortuosity while feeding", y = "Density") +
  #scale_fill_brewer(palette = "Set2") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")


# Plot B: Avg Feeding Speed
speed_plot <- ggplot(SRW_dive_data, aes(x = avg_feeding_speed, color = deployment)) +
  geom_density(alpha = 0.6) +
  geom_density(color = "black", size = 1) +
  geom_rug(aes(x = avg_feeding_speed), sides = "b", alpha = 0.3) +
  labs(x = "Avg speed while feeding (m/s)", y = "Density") +
  #scale_fill_brewer(palette = "Set2") +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom")

# Combine the four plots using ggarrange and add individual labels (A, B)
combined_plot <- ggarrange(tort_plot, speed_plot,
                           ncol = 1, nrow = 2, 
                           labels = c("A", "B"),  # Add labels A, B
                           align = "v", 
                           common.legend = TRUE, legend = "bottom")

# Display the combined plot
print(combined_plot)


ggsave("Feeding_tort and speed_plot.pdf", 
       width = 9, height = 5, units = "in")   










  
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
  
  

