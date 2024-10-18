################################
# Prelim tagging analysis
################################

Tag_guide_SRW <- readxl::read_xlsx("Tag_Guide_SRW.xlsx") %>% 
  mutate(Total_tag_time = as.POSIXct(`Total Tag On Time HH:MM:SS _`, format = "%H:%M:%S", tz = "UTC", origin = "1970-01-01"))

  mutate(Total_tag_time = as.POSIXct(`Total Tag On Time HH:MM:SS _`, format = "%H:%M:%S"))


  
#Dive depth by individual----
  ggplot(SRW_dive_data, aes(x = avg_feeding_depth, color = deployment)) +
    geom_density(alpha = 0.5) + 
    geom_density(aes(x = avg_feeding_depth), color = "black", size = 1) +
    labs(x = "Average Feeding Depth", y = "Density") +
    theme_minimal()
  
#Dive depth by time of day----  
  # Make sure the 'time' column is in POSIXct format
  SRW_dive_data <- SRW_dive_data %>%
    mutate(time = as.POSIXct(time, format="%H:%M:%S"))
  
  # Create the plot
  ggplot(SRW_dive_data, aes(x = time, y = avg_feeding_depth, color = deployment)) +
    geom_point(alpha = 0.7) +
    labs(title = "Scatter plot of Average Feeding Depth by Time and Deployment",
         x = "Time of day",
         y = "Average Feeding Depth") +
    scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +  # Setting x-axis ticks every hour
    ylim(0,70) +
    scale_y_reverse() +
    theme_classic(base_size = 16)
  
#Feeding time per dive by time of day----  

  # Create the plot
  ggplot(SRW_dive_data, aes(x = feeding_time, y = avg_feeding_depth, color = deployment)) +
    geom_point(alpha = 0.7) +
    labs(title = "Scatter plot of Average Feeding Depth by Time and Deployment",
         x = "Feeding time (s)",
         y = "Average Feeding Depth") +
    #scale_x_datetime(date_breaks = "1 hour", date_labels = "%H:%M") +  # Setting x-axis ticks every hour
    ylim(0,70) +
    scale_y_reverse() +
    theme_classic(base_size = 16)
  
  
  

  
  
  
  
  
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
  
  

